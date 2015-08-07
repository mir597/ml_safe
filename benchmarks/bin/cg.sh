#!/bin/bash

BENCHMARK_HOME=${JS_HOME}/benchmarks
jsaf=${JS_HOME}/bin/jsaf

misscond="0 0 0 [0-9]"
allcond="[0-9] [0-9] [0-9] [0-9]"

color_code=(
	"\033[1;35m" # text_s
       	"\033[37m"   # text
	"\033[1;32m" # info_s
	"\033[0;32m" # info
	"\033[1;31m" # warn_s
	"\033[0;31m" # warn
	)

colorize() {
	read -r headers
	echo -e "$headers"

	printf '%*s\n' "${#headers}" ' ' | tr ' ' "="
	declare -i i
	while read line;do
		i=$i+1
		if (( $i%2 == 1 )); then
			msg text_s "$line"
		else
			msg text "$line"
		fi
	done
}

msg() {
	if [ -n "$NO_ANSI" ];then
		shift
		echo -e "$@"
	else
		c=$1
		shift
		case $c in
			text_s) echo -e "${color_code[0]}$@\033[m";;
			text) echo -e "${color_code[1]}$@\033[m";;
			info_s) echo -e "${color_code[2]}$@\033[m";;
			info) echo -e "${color_code[3]}$@\033[m";;
			warn_s) echo -e "${color_code[4]}$@\033[m";;
			warn) echo -e "${color_code[5]}$@\033[m";;
			*) echo -e "$@";;
		esac
	fi	
}

pp_number() {
	if [[ -z "$1" ]];then
		echo -n "_"
	else
		[[ -n "$2" ]] || echo -n "$1"
		[[ -z "$2" ]] || echo -n "$(printf "%.$2f" $1)"
	fi
}

pp_diffnumber() {
	if [[ -z "$1" ]];then
		echo -n "_"
	else
		[[ -n "$2" ]] || echo -n "$1"
		[[ -z "$2" ]] || echo -n "$(printf "%+.$2f" $1)"
	fi
}

pp_diff() {
	if [ "$1" == "$2" ];then
		return
	fi
	if [ "$1" == "NaN" ];then
		r=$2
	elif [[ -n "$2" ]];then
		r=$(echo "$1 - $2" | bc 2> /dev/null)
	fi
	[[ -z $r ]] || echo -n "(`pp_diffnumber "$r" $3`)"
}

usage_run () {
	cat << EOF
Usage: `basename $0` [-h] [-d] TARGET
Runs the analysis for a single TARGET, and records the result.
TARGET must be a path for a root of the target benchmark.
Runs type analysis for every single .js file in the LIST, and records the result.

  -h    Display this help and exit.
  -d    Records all the result messages for debugging.
EOF
	exit
}

usage_runs () {
	cat << EOF
Usage: `basename $0` [-h] [-d] LIST
Runs the analysis for benchmarks in the given LIST, and records the result.
LIST is a text file containing a list of benchmarks which is a path for a root of each benchmark.  LIST must be new line separated and can use # style comments.
A default value for LIST is "${BENCHMARK_HOME}/cg.list"

  -h    Display this help and exit.
  -d    Records all the result messages for debugging.
EOF
	exit
}

run () {
	while getopts hd OPT;do
		case "$OPT" in
			h) usage_run;;
			d) s_debug=true;;
		esac
	done
	shift `expr $OPTIND - 1`

	name=${1##*/}
	out="result_$name.out"

	msg info "- $name"
	if [[ -z $s_debug ]];then
		$jsaf analyze -result $1/dynamic-cg.fixed.json -out $out $1/*.js
	else
		msg info "* Debug mode"
		$jsaf analyze -result $1/dynamic-cg.fixed.json -debug -out $out $1/*.js
	fi
	echo "Generated outputs: $out"
}

runs () {
	while getopts hd OPT;do
		case "$OPT" in
			h) usage_runs;;
			d) s_debug="-d";;
		esac
	done
	shift `expr $OPTIND - 1`

	target=${BENCHMARK_HOME}/$1
	[ ! -z $1 ] || target=${BENCHMARK_HOME}/cg.list
	msg info_s "* Target: $target"
	list=`cat $target | grep '^[^#]*' -o`

	for v in $list;do
		msg info " $v"
	done

	for v in $list;do
		run $s_debug ${BENCHMARK_HOME}/$v
	done
}

showstat () {
	name=${1}
	read miss <<< $(grep "$misscond :1" $name | wc -l)
	read all <<< $(grep "$allcond :1" $name | wc -l)
	let hit="$all - $miss"
	read fa <<< $(grep "$allcond :0" $name | grep -v "$misscond" | wc -l)
	per=$(($hit * 100 / $all))
	alarms=$(($hit + $fa))
	prec=$(($hit * 100 / $alarms))
	echo -n "$name"
	echo -n ",`pp_number "${hit}"`"
	echo -n ",`pp_number "${alarms}"`"
	echo -n ",`pp_number "${all}"`"
	echo -n ",`pp_number "${prec}" 2`"
	echo -n ",`pp_number "${per}" 2`"
	echo ""
}

showstats () {
	(echo "name,hit,alarms,all,precision(%),recall(%)";
	(for v in `ls result_*.out`;do
		showstat $v
	done) | sort -t , -gk 1) | column -t -s , | colorize
}

walarun () {
	name=${1##*/}
	out="wala_$name.out"

	$jsaf analyze -result $1/dynamic-cg.fixed.json -wala $1/optimistic-cg.fixed.json -debug -out $out $1/*.js
}

walaruns () {
	target=${BENCHMARK_HOME}/$1
	[ ! -z $1 ] || target=${BENCHMARK_HOME}/cg.list
	msg info_s "* Target: $target"
	list=`cat $target | grep '^[^#]*' -o`

	for v in $list;do
		msg info " $v"
	done

	for v in $list;do
		walarun $s_debug ${BENCHMARK_HOME}/$v
	done
}

comparewala () {
	while getopts d OPT;do
		case "$OPT" in
			d) NO_ANSI=true;;
		esac
	done

	msg info "========== -:worse, +:better, =:worse false alarms =========="
	for v in `ls wala_*.out`;do
		msg info "* $v"
		grep "$misscond :1 1" $v | while read m;do echo "-$m"; done
		grep "$allcond :1 0" $v | grep -v "$misscond :" | while read m;do echo "+$m"; done
		grep "$allcond :0 0" $v | grep -v "$misscond :" | while read m;do echo "=$m"; done
	done
}

cmd=`basename $0`

case $cmd in
	"run" | "runs" | "showstat" | "showstats" | "walarun" | "walaruns" | "comparewala" )
		$cmd $@;;
	*) exit;;
esac

