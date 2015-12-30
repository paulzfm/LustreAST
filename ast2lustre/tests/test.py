import os

if (os.path.isfile("result")):
	os.remove(result)
if (not os.path.isdir("result")):
	os.mkdir("result")

os.chdir("../src")

astFolder = "../tests/ast"
lustreFolder = "../tests/lustre"
fileList = os.listdir(astFolder)

foutHead = open("../tests/result.txt",'w')

matchNum = 0
mistakeNum = 0
lackNum = 0
for fileName in fileList:
	fName = fileName[:len(fileName)-4]
	astData = os.popen("python lexer.py -i "+astFolder+"/"+fName+".ast | ./ast").readlines()
	astData = reduce(lambda x,y:x+y, astData)
	lustreFile = lustreFolder+"/"+fName+".lustre"
	if (os.path.isfile(lustreFile)):
		fin = open(lustreFile,'r')
		lustreData = fin.readlines()
		lustreData = reduce(lambda x,y:x+y, lustreData)
		fin.close()

		astData = astData.replace('\n','').replace('\r','').replace('\t','').replace(' ','')
		lustreData = lustreData.replace('\n','').replace('\r','').replace('\t','').replace(' ','')

		astData = astData.replace('(','').replace(')','')
		lustreData = lustreData.replace('(','').replace(')','')

		astData = astData.replace('node','').replace('function','')
		lustreData = lustreData.replace('node','').replace('function','').replace('private','')

		if (astData != lustreData):
			print fName+"\t: mistake"
			fout = open("../tests/result/"+fName+".result",'w')
			fout.write("ast:   \t")
			fout.write(astData+'\n')
			fout.write("lustre:\t")
			fout.write(lustreData+'\n')
			fout.close()
			foutHead.write(fName+"\t: mistake"+"\n")
			foutHead.write("ast:   \t")
			foutHead.write(astData+'\n')
			foutHead.write("lustre:\t")
			foutHead.write(lustreData+"\n\n")
			mistakeNum = mistakeNum + 1

		else:
			print fName+"\t: match"
			foutHead.write(fName+"\t: match\n\n")
			matchNum = matchNum + 1
	else:
		print fName+"\t: lack lustre file"
		foutHead.write(fName+"\t: lack lustre file\n\n")
		fout = open("../tests/result/"+fName+".output",'w')
		fout.write(astData+'\n')
		fout.close()
		lackNum = lackNum + 1
foutHead.close()
print "match: "+str(matchNum)
print "mistake: "+str(mistakeNum)
print "lack lustre file: "+str(lackNum)