import os
import subprocess
import re
import sys
import time
import common

path = os.getcwd() + "/"
EXS_DIR = path + "io-trio"
ref_dir = path + "ref-trio"
ref_result_dir = path + "ref-results"

# for equivalence checking
sol_dir = path + "checker"
ref_bench_dir = path + "benchmarks/ref"

benchmarks = [
    "bool_always_false",
    "bool_always_true",
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
    "tree_collect_leaves",
    "tree_count_leaves",
    "tree_count_nodes",
    "tree_inorder",
    "tree_postorder",
    "tree_preorder",
    "tree_nodes_at_level",
    "tree_map",
    "tree_binsert",
]

benchmarks = [
    # "bool_always_false",
    # "bool_always_true",
    "bool_band",
    "bool_bor",
    "bool_impl",
    # "bool_neg",
    "bool_xor",
    "list_append",
    "list_compress",
    "list_concat",
    "list_drop",
    "list_even_parity",
    "list_filter",
    "list_fold",
    "list_hd",
    "list_inc",
    "list_last",
    "list_length",
    "list_map",
    "list_nth",
    "list_pairwise_swap",
    "list_rev_append",
    "list_rev_fold",
    "list_rev_snoc",
    "list_rev_tailcall",
    "list_snoc",
    "list_sort_sorted_insert",
    "list_sorted_insert",
    "list_stutter",
    "list_sum",
    "list_take",
    "list_tl",
    "nat_add",
    "nat_iseven",
    "nat_max",
    "nat_pred",
    "tree_binsert",
    "tree_collect_leaves",
    "tree_count_leaves",
    "tree_count_nodes",
    "tree_inorder",
    "tree_map",
    "tree_nodes_at_level",
    "tree_postorder",
    "tree_preorder",
]

benchmarks = ["list_compress"]

def run():
    for name in benchmarks:
        print("Running " + name)

        with open(os.path.join(EXS_DIR, name), 'r') as f_io:
                lines = f_io.readlines()
        examples = [line.rstrip() for line in lines]
        exm = ' '.join(examples)
        # print(exm)
        # print(str(examples))
        start = time.time()
        cmd = 'time timeout 120 ./syrup/syrup syrup ' + name + ' "' + exm + '"'
        # print(cmd)
        proc = subprocess.run(cmd,capture_output=True, text=True, shell=True)
        print(proc.stdout)
        # print(proc.stderr)
        # exit(0)
        end = time.time()
        t = end - start
        print("Time taken: " + str(t))
        # with open("results.txt", "a") as f:
        #     f.write(name + ", " + str(t) + "\n")
        # print("")

def run_equiv(iter=1):
     #file exists
     if os.path.exists("syrup_" + str(iter) + "_equiv_time.txt"):
            os.remove("syrup_" + str(iter) + "_equiv_time.txt")
     for name in benchmarks:
        # equiv1 : reference
        # equiv2 : candidate
        print("Check Equiv: " + name)
        start = time.time()
        if iter == 1:
            cmd = "burst/BurstCmdLine.exe -check-equiv1 checker/" + name + ".sol -check-equiv2 default.sol benchmarks/ref/" + name + ".mls"
        else:
            cmd = "burst/BurstCmdLine.exe -check-equiv1 checker/" + name + ".sol -check-equiv2 syrup_experiment/ref-results/synth" + str(iter-1)+ "/" + name + ".sol benchmarks/ref/" + name + ".mls 2>&1"
        proc = subprocess.run(cmd,capture_output=True, text=True, shell=True)
        end = time.time()
        t = end - start
        print(proc.stdout)
        print("Time taken: " + str(t))
        with open("syrup_" + str(iter) + "_equiv_time.txt", "a") as f:
            f.write(name + ", " + str(t) + "\n")

def run_ref(iter=1):
     if os.path.exists("syrup_" + str(iter) + "_synth_time.txt"):
            os.remove("syrup_" + str(iter) + "_synth_time.txt")
     for name in benchmarks:
        print("Running " + name)
        with open(os.path.join(ref_dir + "/iter" + str(iter), name), 'r') as f_io:
                lines = f_io.readlines()
        examples = [line.rstrip() for line in lines]
        exm = ' '.join(examples)
        # print(exm)
        # print(str(examples))
        start = time.time()
        cmd = 'timeout 120 ./syrup/syrup syrup ' + name + ' "' + exm + '"'
        # print(cmd)
        proc = subprocess.run(cmd,capture_output=True, text=True, shell=True)
        print(proc.stdout)
        print(proc.stderr)
        # exit(0)
        end = time.time()
        t = end - start
        print("Time taken: " + str(t))
        with open("syrup_" + str(iter) + "_synth_time.txt", "a") as f:
            f.write(name + ", " + str(t) + "\n")
        with open(ref_result_dir + "/synth" + str(iter) + "/" + name + ".sol", "a") as f:
            f.write(proc.stdout)
        # print("")

def make_default_sol():
    for name in benchmarks:
        with open(sol_dir + "/" + name + ".sol", 'r') as file:
            lines = file.readlines()

        # 첫 2줄만 남기고 나머지 줄 삭제
        lines = lines[:2]

        # 파일을 덮어쓰며 수정
        # print(ref_result_dir)
        with open(ref_result_dir + "/synth1/" + name + ".sol", 'w') as f:
            f.writelines(lines)

# def temp():
#      dir_path = path + "syrup_experiment/ref-results/synth2"
#      for filename in os.listdir(dir_path):
#         # 파일 경로 생성
#         old_file_path = os.path.join(dir_path, filename)

#         # 파일인지 확인
#         if os.path.isfile(old_file_path):
#             # '_1'을 포함한 파일 이름만 처리
#             if '_1' in filename:
#                 # '_1'을 제거한 새 파일 이름 생성
#                 new_filename = filename.replace('_1', '')

#                 # 새 파일 경로
#                 new_file_path = os.path.join(dir_path, new_filename)

#                 # 파일 이름 변경
#                 os.rename(old_file_path, new_file_path)

def run_test_trio():
    for name in benchmarks:
        print("Running " + name)

        with open(os.path.join(EXS_DIR, name), 'r') as f_io:
                lines = f_io.readlines()
        examples = [line.rstrip() for line in lines]
        exm = ' '.join(examples)
        p = common.run_trio(name, exm)
        print(p.stdout)

def main():
    # run()
    # make_default_sol()
    # run_equiv(4)
    # run_ref(3)
    run_test_trio()

if __name__ == "__main__":
    main()

# run_ref 전에 synth 폴더 생성 필요 - save_ref_results 복사해오기