#!/usr/bin/env python3
import os
import argparse
import csv
from collections import defaultdict
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.ticker as mtick
import matplotlib.lines as mlines
from common import benchmarks, benchmarks_nonrec, \
    draw_hlines, grouper, mean_of_matrix

matplotlib.use('agg')
matplotlib.rc('font', size=7)
plt.rcParams.update({
    "text.usetex": True,
    "font.family": "Helvetica"
})
# plt.style.use('_mpl-gallery')

target_benchmarks = [
    "bool_band",
    "bool_neg",
    "bool_xor",
    "nat_add",
    "nat_iseven",
    "nat_max",
    "list_append",
    "list_rev_tailcall",
    "list_sort_sorted_insert",
    "tree_count_nodes",
    "tree_preorder",
    "tree_binsert",
]
# img for random testing of trio and syrup
LOG_DIR = "T_Random+BC"

parser = argparse.ArgumentParser()
parser.add_argument("--rec",
                    help="only consider recursive tasks",
                    action="store_true")
parser.add_argument("--ablation",
                    help="include SyRup* for abalation study",
                    action="store_true")
parser.add_argument("--trio",
                    help="use trio data",
                    action="store_true")
args = parser.parse_args()


def summary_learnability(benchmark_names, tool1, tool2, learnability1, learnability2):
    print(f'{tool1} needs more examples than {tool2} to achieve 50% on \
    {sum(x[4] > y[4] for x, y in zip(learnability1, learnability2))} \
    tasks')
    print(', '.join([benchmark_names[i]
                     for i, (x, y) in
                     enumerate(zip(learnability1, learnability2))
                     if x[4] > y[4]]))
    print(f'{tool1} needs more examples than {tool2} to achieve 90% on \
    {sum(x[8] > y[8] for x, y in zip(learnability1, learnability2))} \
    tasks')
    print(f'{tool1} needs less examples than {tool2} to achieve 50% on \
    {sum(x[4] < y[4] for x, y in zip(learnability1, learnability2))} \
    tasks')
    print(', '.join([benchmark_names[i]
                     for i, (x, y) in
                     enumerate(zip(learnability1, learnability2))
                     if x[4] < y[4]]))
    print(f'{tool1} needs less examples than {tool2} to achieve 90% on \
    {sum(x[8] < y[8] for x, y in zip(learnability1, learnability2))} \
    tasks')


with open(os.path.join(LOG_DIR, 'result.csv'), 'r') as f:
    for linecount, _line in enumerate(f):
        pass

if args.ablation and linecount % 18 == 0:
    colors = ['tab:blue', 'tab:red', 'tab:orange', 'tab:green']
    linestyles = ['-', '-', '--', '-.']
    tools = [r'$\textsc{SyRup}$', r'$\textsc{SyRup}^*$',
             r'$\textsc{Burst}$', r'$\textsc{SMyth}$']
elif args.trio:
    colors = ['tab:red', 'tab:blue']
    linestyles = ['--', '-',]
    tools = [r'$\textsc{SyRup}$',
             r'$\textsc{Trio}$']
    alphas = [1.0, 0.6]
elif not args.ablation:
    colors = ['tab:blue', 'tab:orange', 'tab:green']
    linestyles = ['-', '--', '-.']
    tools = [r'$\textsc{SyRup}$',
             r'$\textsc{Burst}$', r'$\textsc{SMyth}$']

else:
    raise ValueError('Wrong number of lines in result.csv')

FIG_NAME = "random-quility.pdf"
# FIG_NAME = "learnability{}{}.pdf".format(
#     "-ablation" if args.ablation else "",
#     "-rec" if args.rec else "")

# 총 횟수. 
MAX_CARD = 20  # will be read from result.csv
cards = range(0, MAX_CARD+1)

cutoffs = [i / 10 for i in range(1, 11)]
nexamples = range(1, 9)
learnability = defaultdict(list)

# NUM_OF_COLS = 5
# NUM_OF_ROWS = -(-len(benchmarks) // NUM_OF_COLS)
NUM_OF_COLS = 7
NUM_OF_ROWS = 2
fig, axs2d = plt.subplots(NUM_OF_ROWS, NUM_OF_COLS, figsize=(8,2), sharex=True, sharey=True)
axs = axs2d.flatten()
print(len(axs))
# gs = fig.add_gridspec(len(benchmarks) + 1, hspace=0)
# fig = plt.figure(figsize=(FIG_WIDTH, len(benchmarks)))
# gs = fig.add_gridspec(len(benchmarks) + 1, hspace=0)
# axs = gs.subplots(sharex=True)

with open(os.path.join(LOG_DIR, 'result.csv'), 'r') as f:
    rows = csv.reader(f, delimiter=',')
    header = next(rows)
    MAX_CARD = int(header[-1])
    # print(linecount)

    prefix_last = ""
    i = 0
    rows_to_be_separated = []
    for (name, count, *data) in grouper(
            # 15 if linecount % 15 == 0 else 18, rows):
            11, rows):
        if args.rec and name[0] in benchmarks_nonrec:
            continue
        if name[0] not in target_benchmarks:
            continue
        # get correct counts
        corrects = data[:2]
        # print(len(corrects[0]))
        # if linecount % 15 == 0:
        #     corrects = data[:3]
        # else:
        #     corrects = data[:4]
        #     if not args.ablation:
        #         del corrects[1]
        if i == 6: axs[i].axis('off');i += 1
        # get prefix that ends with underscore in name
        prefix = name[0][:name[0].find('_')]
        if prefix == prefix_last:
            prefix_last = prefix
            # for j in range(i, (i + 4) // 5 * 5):
            #     axs[j].axis('off')
            i = (i + 4) // 5 * 5
            if i != 0:
                rows_to_be_separated.append(i//5-1)
        # print(i)
        # if i == 6: i += 1;continue
        # axs[6].axis('off')
        print(name[0])
        # get number of trace-complete examples
        # start with 3
        n = next((i for i, v in enumerate(count[3:]) if int(v) == 0), MAX_CARD)
        # print(prefix)
        # print(count[2:])
        # n은 1, 2, 3, 4 ... # of examples

        # axs[i].xaxis.set_major_formatter(mtick.PercentFormatter(1.0))
        plt.xticks(nexamples, nexamples)
        axs[i].yaxis.set_major_formatter(mtick.PercentFormatter(1.0))
        # axs[i].set_yticks([0, n])
        # axs[i].set_yticks([0, 1])
        axs[0].set_yticks([0, 0.5, 1.0])
        axs[i].set_ylim([0, 1.1])
        axs[i].set_title(name[0])
        # name은 벤치 이름
        # axs[i].text(1.02, .05, f'100\% = {n} IOs',
        #             ha='right', va='bottom', fontsize=7,
        #             bbox={'facecolor': 'white', 'alpha': 0.2,
        #                   'pad': 0.2, 'boxstyle': 'round'})
        for c, ls, tool, correct, alpha in zip(colors, linestyles, tools, corrects, alphas):
            correct = list(map(
                lambda x, y: int(x)/int(y) if int(y) != 0 else 0.0,
                correct[3:11], count[3:11]))
            
            # bench 마다 최대 예제수가 다름. 4~8임
            # success rate 는 correct 에
            # print(correct)
            # print(len(correct))
            # exit(0)
            # get percentage of trace-complete examples
            # 말고 예제 1~8 까지
            # line = []
            # for cutoff in cutoffs:
            # for cutoff in range(1, 9):
            #     line.append(
            #         next((i/n for i, v in enumerate(correct)), 1))
            # learnability[tool].append(line)
            # 이게 그리는거 먼저 syrup 그리겠지 plt.plot(x, syrup_succ, marker='o', label='syrup succ')
            axs[i].plot(nexamples, correct, label=tool, linestyle=ls, color=c, alpha=alpha)

        i += 1
    rows_to_be_separated.append(i//5)
rows_to_be_separated=[0,1]
axs[i].legend(
    [mlines.Line2D([], [], color=c, linestyle=ls)
     for c, ls in zip(colors, linestyles)], tools,
    loc='lower right', fontsize=7)

for j in range(i, len(axs)):
    axs[j].axis('off')

# print(len(axs))
fig.supylabel('Success Rate')
# fig.supylabel('percentage of trace-complete examples')
fig.supxlabel('Num of Examples')
fig.tight_layout()
# print("what")
# print(axs2d.shape)
# print(len(axs2d.flat))
draw_hlines(fig, axs2d, rows_to_be_separated)
# fig.savefig("learnability-individual{}{}.pdf".format(
#     "-ablation" if args.ablation else "",
#     "-rec" if args.rec else ""), bbox_inches='tight')
fig.savefig(FIG_NAME, bbox_inches='tight')
