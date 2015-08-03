#!/bin/bash

BENCHMARK_HOME=${JS_HOME}/benchmarks
jsaf=${JS_HOME}/bin/jsaf

color_code=(
	"\033[1;35m" # text_s
       	"\033[37m"   # text
	"\033[1;32m" # info_s
	"\033[0;32m" # info
	"\033[1;31m" # warn_s
	"\033[0;31m" # warn
	)

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

cmd=`basename $0`

case $cmd in
	"run" | "runs" )
		$cmd $@;;
	*) exit;;
esac

