import os

if (os.path.isfile("result")):
	os.remove(result)
if (not os.path.isdir("result")):
	os.mkdir("result")

astFolder = "./ast"
lustreFolder = "./lustre"
fileList = os.listdir(lustreFolder)

foutHead = open("./result.txt",'w')

matchNum = 0
mistakeNum = 0
lackNum = 0
for fileName in fileList:
	fName = fileName[:len(fileName)-7]
	print fName
	lustreData = os.popen("cat "+lustreFolder+"/"+fName+".lustre | ../src/lustre").readlines()

	if (len(lustreData) > 1):
		lustreData = reduce(lambda x,y:x+y, lustreData)
	elif (len(lustreData) == 1):
		lustreData = lustreData[0]
	else:
		mistakeNum = mistakeNum + 1
		foutHead.write(fName+"\t: mistake"+"\n")
		foutHead.write("No output\n\n")
		continue
	astFile = astFolder+"/"+fName+".ast"
	if (os.path.isfile(astFile)):
		fin = open(astFile,'r')
		astData = fin.readlines()
		astData = reduce(lambda x,y:x+y, astData)
		fin.close()

		astData = astData.replace('\n','').replace('\r','').replace('\t','').replace(' ','')
		lustreData = lustreData.replace('\n','').replace('\r','').replace('\t','').replace(' ','')

#		astData = astData.replace('(','').replace(')','')
#		lustreData = lustreData.replace('(','').replace(')','')

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
print "total: "+str(len(fileList))
print "match: "+str(matchNum)
print "mistake: "+str(mistakeNum)
print "lack lustre file: "+str(lackNum)