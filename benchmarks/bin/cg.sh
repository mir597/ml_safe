#!/bin/bash

BENCHMARK_HOME=${JS_HOME}/benchmarks
jsaf=${JS_HOME}/bin/jsaf
RESULT_HOME=${BENCHMARK_HOME}/results

misscond=""
allcond=""

init_cond() {
	c=`head -n 4 $1 | grep -o "\([0-9] \)\([0-9] \)*:" | grep -o "[0-9]* " | wc -l`
	c=$(echo ${c})
	misscond="\(0 \)\{$c\}:"
	allcond="\([0-9] \)\{$c\}:"
}

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
	read -r headers
	echo -e "$headers"

#	printf '%*s\n' "${#headers}" ' ' | tr ' ' "="
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
	while getopts hod OPT;do
		case "$OPT" in
			h) usage_run;;
			d) s_debug=true;;
			o) s_ours=true;;
		esac
	done
	shift `expr $OPTIND - 1`

	name=${1##*/}
	out="result_$name.out"
	dcg="dynamic-cg.fixed.json"
	if [[ ! -z $s_ours ]];then
		dcg="dcg.pretty.json"
	fi

	msg info "- $name"
	if [[ -z $s_debug ]];then
		$jsaf analyze -result $1/$dcg -out $out $1/*.js
	else
		msg info "* Debug mode"
		$jsaf analyze -result $1/$dcg -debug -out $out $1/*.js
	fi
	echo "Generated outputs: $out"
}

runs () {
	while getopts hod OPT;do
		case "$OPT" in
			h) usage_runs;;
			o) s_ours="-o";;
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
		run $s_debug $s_ours ${BENCHMARK_HOME}/$v
	done
}

showstat () {
	name=${1}
	init_cond ${1}
	miss=`grep -c "${misscond}1" $name`
	all=`grep -c "${allcond}1" $name`
	let hit="$all - $miss"
	fa=`grep "${allcond}0" $name | grep -c -v "${misscond}"`
	if (( $all == 0 ));then
		per="NaN"
	else
		per=$(($hit * 100 / $all))
	fi
	alarms=$(($hit + $fa))
	if (( $alarms == 0 ));then
		perc="NaN"
	else
		prec=$(($hit * 100 / $alarms))
	fi
	echo -n "$name"
	echo -n ",`pp_number "${hit}"`"
	echo -n ",`pp_number "${alarms}"`"
	echo -n ",`pp_number "${all}"`"
	echo -n ",`pp_number "${prec}" 2`"
	echo -n ",`pp_number "${per}" 2`"
	echo ""
}

createfolder () {
	declare -i i
	i=0
	date=`date +'%Y-%m-%d'`
	name=${RESULT_HOME}/$date

	# create a folder for result data.
	while [ -e $name ];do
		name="${RESULT_HOME}/$date.$i"
		i=$i+1
	done
	mkdir -p $name

	echo $name
}

showstats () {
	(echo "name,hit,alarms,all,precision(%),recall(%)";
	(for v in `ls result_*.out`;do
		showstat $v
	done) | sort -t , -gk 1) | formatting | colorize
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

walastat () {
	name=${1}
	t=$(grep "# RESULT: " $name)
	init_cond ${1}

	if [ -z "$t" ];then
		miss=`grep -c "${misscond}1" $name`
		all=`grep -c "${allcond}1" $name`
		let hit="$all - $miss"
		fa=`grep "${allcond}0" $name | grep -c -v "${misscond}"`
		per=$(($hit * 100 / $all))
		alarms=$(($hit + $fa))
		prec=$(($hit * 100 / $alarms))
		walaonly=`grep -c "${misscond}1 1" $v`
		safeonly=`grep "${allcond}1 0" $v | grep -c -v "${misscond}"`
		safef=`grep "${allcond}0" $v | grep -c -v "${misscond}"`
		walaf=`grep -c "${allcond}0 1" $v`
		safewalaf=`grep "${allcond}0 1" $v | grep -c -v "${misscond}"`
		union=$(($safef + $walaf - $safewalaf))
		result=""
		result="${result}${name}"
		result="${result},`pp_number "${hit}"`"
		result="${result},`pp_number "${alarms}"`"
		result="${result},`pp_number "${all}"`"
		result="${result},`pp_number "${prec}" 2`"
		result="${result},`pp_number "${per}" 2`"
		result="${result},`pp_number "${safeonly}"`"
		result="${result},`pp_number "${walaonly}"`"
		result="${result},`pp_number "${safef}"`"
		result="${result},`pp_number "${walaf}"`"
		result="${result},`pp_number "${safewalaf}"`"
		result="${result},`pp_number "${union}"`"
		echo "# RESULT: ${result}" >> $name
		echo ${result}
	else
		echo ${t:10}
	fi
}

print_line () {
	v=("$@")
	for j in "${!v[@]}";do
		case ${align[j]} in
			"l") ali="-";;
			*) ali="";;
		esac
		printf "| %$ali${max[j]}s " ${v[j]}
	done
	echo "|"
}

formatting () {
	sep=","
	declare -a list
	let i=0
	declare -a max
	declare -a align
	num='^[0-9]+$'
	float='^[0-9]+([.][0-9]*)$'
	numorfloat='^[0-9]+([.][0-9]*)?$'
	while IFS=$'\n' read -r line; do
		list[i]=${line}
		IFS=','
		read -a v <<< "$line"
		for idx in "${!v[@]}";do
			if ! [[ ${max[idx]} =~ $num ]];then
				max[idx]=0
			fi
			if (( ${max[idx]} < ${#v[idx]} )); then
				max[idx]=${#v[idx]}
			fi
			if [[ ${v[idx]} =~ $num ]];then
				form[idx]=1
				align[idx]="r"
			elif [[ ${v[idx]} =~ $float ]];then
				form[idx]=${#BASH_REMATCH[1]}
				align[idx]="r"
			else
				form[idx]=0
				align[idx]="l"
			fi
			sum[idx]=0
		done
		((++i))
	done
	all=${i}

	for i in "${!list[@]}";do
		read -a v <<< "${list[i]}"
		print_line ${v[@]}
		if ((i == 0));then
# Print the second head line.
			for j in "${!v[@]}";do
				echo -n "|"
				case ${align[j]} in
					"l") echo -n ":";;
					*) ;;
				esac
				s=$((${max[j]}+1))
				printf '%*s' "${s}" ' ' | tr ' ' "-"
				case ${align[j]} in
					"r") echo -n ":";;
					*) ;;
				esac
			done
			echo "|"
		else
			for j in "${!v[@]}";do
				if [[ ${v[j]} =~ $numorfloat ]]; then
					scale=$((${form[j]} - 1))
					if (($scale >= 0));then
						sum[j]=$(echo "scale=${scale};${sum[j]} + ${v[j]}" | bc)
					fi
				fi
			done
		fi
	done
	for j in "${!v[@]}";do
		scale=$((${form[j]} - 1))
		if (($scale >= 0));then
			a=$(echo "scale=${scale};${sum[j]} / ($all - 1)" | bc)
		else
			a="-"
		fi
		avg[j]=$a
	done
# Sum
	sum[0]="Sum"
	print_line ${sum[@]}
# Average
	avg[0]="Average"
	print_line ${avg[@]}
}

comparewala () {
	while getopts dv OPT;do
		case "$OPT" in
			d) NO_ANSI=true;;
			v) VERBOSE=true;;
		esac
	done

	(echo "name,hit,alarms,all,prec(%),recall(%),SAFE(t),WALA(t),SAFE(f),WALA(f),(f)∩ (f),(f)∪ (f)";
	(for v in `ls wala_*.out`;do
		walastat $v
	done) | sort -t , -gk 1) | formatting | colorize
#	done) | sort -t , -gk 1) | column -t -s , | colorize

#	msg info "========== -:worse, +:better, =:worse false alarms =========="
#	for v in `ls wala_*.out`;do
#		msg info "* $v"
#		grep -c "${misscond}1 1" $v | while read m;do echo "- $m"; done
#		grep "${allcond}1 0" $v | grep -c -v "${misscond}" | while read m;do echo "+ $m"; done
#		grep "${allcond}0 0" $v | grep -c -v "${misscond}" | while read m;do echo "= $m"; done
#		grep "${misscond}1 1" $v | while read m;do echo "-$m"; done
#		grep "${allcond}1 0" $v | grep -v "${misscond}" | while read m;do echo "+$m"; done
#		grep "${allcond}0 0" $v | grep -v "${misscond}" | while read m;do echo "=$m"; done
#	done
}

cmd=`basename $0`

case $cmd in
	"run" | "runs" | "showstat" | "showstats" | "walarun" | "walaruns" | "comparewala" )
		$cmd $@;;
	*) exit;;
esac

