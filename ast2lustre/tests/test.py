import os

if (os.path.isfile("result")):
	os.remove(result)
if (not os.path.isdir("result")):
	os.mkdir("result")

os.chdir("../src")

astFolder = "../tests/ast"
lustreFolder = "../tests/lustre"
fileList = os.listdir(astFolder)

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

		if (astData != lustreData):
			print fName+"\t: mistake"
			fout = open("../tests/result/"+fName+".result",'w')
			fout.write(astData+'\n')
			fout.write(lustreData+'\n')
			fout.close()
		else:
			print fName+"\t: match"
	else:
		print fName+"\t: lack lustre file"
		fout = open("../tests/result/"+fName+".output",'w')
		fout.write(astData+'\n')
		fout.close()

