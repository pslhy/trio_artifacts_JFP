import matplotlib.pyplot as plt
import pandas as pd
import sys
# $1: typ
# $2 ~ $n csv files

# typ : io | ref
# data_name = [['EUSolver','CVC4','Euphony','Duet'],['^','D', 'X', 'o'],['g','r','m','b']]
# data_name = [['$\textsc{Trio}^{B}$','$\textsc{Trio}^{L}$','$\textsc{Trio}^{--}$','$\textsc{Trio}$'],['^','v','D','o'],['g','m','r','b'],['none','none','none','none']]

# typ = sys.argv[1]
# datas = len(sys.argv) - 2

params = {'legend.fontsize': '18',
         'figure.figsize': (9, 6),
         'axes.labelsize': '20',
         'axes.titlesize':'30',
         'lines.linewidth' : '2',
         'xtick.labelsize':'17',
         'ytick.labelsize':'17'}

plt.rcParams.update(params)


# data : [trio, trio_p, burst, smyth]

plt.title("Solved")
# plt.title("Input-Output Example")
plt.xlabel("Tool")
plt.ylabel("# Solved(# Correct)")

x = ['Trio', 'Trio_p', 'Burst', 'Smyth']
# plt.xlim(30,60)
# plt.xlim(1,15)
# plt.xticks(range(16), range(1,17))

# for i in range(datas):
#     print(globals()['df_{}'.format(i+1)])
# sort and cumulate
# for i in range(datas):
#     globals()['df_{}'.format(i+1)] = globals()['df_{}'.format(i+1)][globals()['df_{}'.format(i+1)]['time'].dropna() < 119.0].sort_values(by='time')

# for i in range(datas):
#     globals()['time_{}'.format(i+1)] = globals()['df_{}'.format(i+1)]['time'].cumsum()

# set data
# for i in range(datas):
#     plt.plot(globals()['time_{}'.format(i+1)], label=data_name[0][i]+' (solved = '+str(globals()['time_{}'.format(i+1)].count())+')', color=data_name[2][i], marker=data_name[1][i], markersize=10, markevery=2, fillstyle=data_name[3][i])

# print(globals()['time_{}'.format(4)])

plt.legend(loc='upper left')
# plt.show()
plt.savefig('./'+typ+'_graph.png')