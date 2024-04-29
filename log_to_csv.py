import sys
import os

# args[1] : log name
args = sys.argv
folder = args[1]
sep = args[2]
# toolname = args[2]
# outfile = args[3]

# test = "expr_boolean.mls.trio_--.csv"

# print (test.split('.'))
# exit()
# f = open(file)
# f.close()
s = "file,size,iter,time,mem\n"
# print(data)
# checker = False
# out = open('log/csv/'+outfile+'.csv','a')
for file in os.listdir(folder) :
    # print(file)
    if file.endswith('.csv'):
        # fname, ext, tool, _ = file.split('.')
        # print(file)
        f = open(folder+'/'+file)
        subject = file.split('.')
        # expr , mls, trio , csv
        # f = open(file)
        data = f.readlines()
        s += subject[0] + ","
        data_str = ','.join(data)
        s += data_str + "\n"
        f.close()
        out = open('log/csv/'+sep+'_'+subject[2]+'.csv','a')
        out.write(s)
        out.close()
        s = ""
    else :
        continue
