import os
import subprocess
import re
import sys
import time

path = os.getcwd() + "/"
EXS_DIR = path + "experiment/io-smyth"
benchmarks = [
    "bool_band",
    "bool_bor",
    "bool_impl",
    "bool_neg",
    "bool_xor",
    "nat_add",
    "nat_iseven",
    "nat_max",
    "nat_pred",
    "list_hd",
    "list_tl",
    "list_last",
    "list_take",
    "list_drop",
    "list_nth",
    "list_sum",
    "list_length",
    "list_even_parity",
    "list_inc",
    "list_stutter",
    "list_snoc",
    "list_append",
    "list_compress",
    "list_concat",
    "list_rev_append",
    "list_rev_fold",
    "list_rev_snoc",
    "list_rev_tailcall",
    "list_pairwise_swap",
    "list_sort_sorted_insert",
    "list_sorted_insert",
    "list_map",
    "list_filter",
    "list_fold",
    "tree_count_leaves",
    "tree_count_nodes",
    "tree_inorder",
    "tree_inorder_bool",
    "tree_postorder",
    "tree_preorder",
    "tree_nodes_at_level",
    "tree_map",
    "tree_binsert",
]

def run():
    for name in benchmarks:
        print("Running " + name)

        with open(os.path.join(EXS_DIR, name), 'r') as f_io:
                lines = f_io.readlines()
        examples = [line.rstrip() for line in lines]
        exm = ' '.join(examples)
        # print(exm)
        # print(str(examples))
        # start = time.time()
        cmd = 'time timeout 120 ./syrup/syrup syrup ' + name + ' "' + exm + '"'
        # print(cmd)
        proc = subprocess.run(cmd,capture_output=True, text=True, shell=True)
        print(proc.stdout)
        # print(proc.stderr)
        # exit(0)
        # end = time.time()
        # print("Time taken: " + str(end - start) + " seconds")
        # print("")

def main():
    run()

if __name__ == "__main__":
    main()