#!/bin/bash
bison yacc_files/$1*.y -o /dev/null -v --report-file="graphs/$1.plg"
		
