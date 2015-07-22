#!/bin/bash

BENCHMARK_HOME=${JS_HOME}/benchmarks
target=${BENCHMARK_HOME}/cg.list
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

run () {
	name=${1##*/}
	out="result_$name.out"

	msg info "- $name"
	$jsaf analyze -result $1/dynamic-cg.fixed.json $1/*.js | tee $out
}

runs () {
	msg info_s "* Target: $target"

	list=`cat $target | grep '^[^#]*' -o`

	for v in $list;do
		msg info " $v"
	done

	for v in $list;do
		run ${BENCHMARK_HOME}/$v
	done
}

cmd=`basename $0`

case $cmd in
	"run" | "runs" )
		$cmd $@;;
	*) exit;;
esac

