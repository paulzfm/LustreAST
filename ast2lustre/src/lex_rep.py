#!/usr/bin/env python2
# -*- coding: utf-8 -*-
file = 'c.txt'
f = open(file).read()
for each in f.splitlines():
	lis = each.split('|') 

def rep(string):
	string = string.replace('$', 'SSS')
	string = string.replace('+', 'ADD')
	string = string.replace('-', 'MINUS')
	string = string.replace('*', 'MUL')
	string = string.replace('/', 'DIVDIV')
	string = string.replace('=', 'EQ')
	string = string.replace('⟨⟩', 'MID')
	string = string.replace('>', 'GRE')
	string = string.replace('>=', 'GREEQ')
	string = string.replace('<', 'LES')
	string = string.replace('<=', 'LESEQ')
	string = string.replace('$', 'SSS')
	string = string.replace('$', 'SSS')
	string = string.replace('$', 'SSS')
	return string

for each in f.splitlines():
    lis = each.split('|')
    for eve in lis:
        print '\t|'+ ' \"'+eve.strip()+'\"' + "\t" + "{ " + rep(eve.strip().upper()) +" }"