#!/bin/bash

for x in ` find shootout/ -name "*.r" `; do 
	echo $x
	rjit -e "source(\"run.r\");run(\"$x\", \"log-test.txt\", \"rjit\", 10)" > /dev/null
	freshr -e "source(\"run.r\");run(\"$x\", \"log-test.txt\", \"gnur\", 10)" > /dev/null
done
