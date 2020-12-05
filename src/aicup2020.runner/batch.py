#!/usr/bin/env python3

import click
import subprocess
import json
import os
import time
import logging
import traceback
import multiprocessing


logging.basicConfig(
    format='%(asctime)s %(levelname)-8s %(message)s',
    level=logging.INFO,
    datefmt='%Y-%m-%d %H:%M:%S')
log = logging.getLogger()


@click.group()
def main():
    pass


def get_player_config(port):
    return {
       "Tcp": {
           "host": None,
           "port": port,
           "accept_timeout": None,
           "timeout": None,
           "token": None
       }
    }


def worker(args):
    idx, p1, p2, lr_bin, start_seed, level, nthreads, count, team_size, profile_mode = args
    port1 = 32003 + idx * 4
    port2 = port1 + 1
    port3 = port1 + 2
    port4 = port1 + 3
    seed = start_seed + idx
    # print([port1, port2])
    # return

    #if level in ["Simple"]:
    #    level_config = "Complex"
    #else:
    #    level_config = {"LoadFrom": {"path": level}}

    swap = idx % 2

    config = {
        "game": {
            "Create": level
        },
        "players": [
            get_player_config(port1),
            get_player_config(port2),
            get_player_config(port3),
            get_player_config(port4),
        ],
        "seed": seed,
    }
    cwd = os.getcwd()

    swap_str = "swap_" if swap else ""

    config_path = os.path.join(cwd, f"tmp/_config{idx}.json")
    result_path = os.path.join(cwd, f"tmp/_{swap_str}result{idx}.txt")
    with open(config_path, "w") as out:
        json.dump(config, out, indent=4)
    if os.path.exists(result_path):
        os.remove(result_path)

    try:
        with subprocess.Popen(f"{lr_bin} --config {config_path} --save-results {result_path} --batch-mode --log-level warn".split(" ")) as process:
            #print("started lr")
            time.sleep(0.5)
            stream = subprocess.DEVNULL
            if profile_mode:
                stream = None
            #print("starting strategies")
            subprocess.Popen([p2 if swap else p1, "127.0.0.1", str(port1), "0000000000000000"], stdout=stream, stderr=stream)
            subprocess.Popen([p2 if swap else p1, "127.0.0.1", str(port2), "0000000000000000"], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            subprocess.Popen([p1 if swap else p2, "127.0.0.1", str(port3), "0000000000000000"], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            subprocess.Popen([p1 if swap else p2, "127.0.0.1", str(port4), "0000000000000000"], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            #print("started processes")
            process.wait()
            #print("waited processes")
            with open(result_path) as result_inp:
                result = json.load(result_inp)                
                res = result['results']
                if swap:
                    (res[0],res[2])=(res[2], res[0])
                    (res[1],res[3])=(res[3], res[1])
                log.info("game with seed=%d results: %d - %d - %d - %d", seed, res[0], res[1], res[2], res[3])
                #print(" - ".join([("CRASHERD " if result["players"][i]["crashed"] else "") + str(result["results"][i]) for i in range(2)]))
    except Exception:
        log.error(traceback.format_exc())
    finally:
        pass


def start_process():
    pass


@main.command()
@click.option('--p1', type=str, required=True, default="C:\\github\\jaina\\src\\aicup2020\\bin\\Release\\netcoreapp5.0\\aicup2020.exe")
@click.option('--p2', type=str, required=True, default="C:\\runner\\versions\\v17\\aicup2020.exe")
@click.option('--lr-bin', type=str, default="C:\\runner\\aicup2020.exe")
@click.option('--start-seed', type=int, default=1)
@click.option('--level', type=str, default="Round1")
@click.option('--nthreads', type=int, default=8)
@click.option('--count', type=int, default=1000)
@click.option('--team-size', type=int, default=2)
@click.option('--profile-mode', is_flag=False)
def run(p1, p2, lr_bin, start_seed, level, nthreads, count, team_size, profile_mode):
    folder  = "tmp"
    if not os.path.exists(folder):
        os.makedirs(folder)

    for filename in os.listdir(folder):
        file_path = os.path.join(folder, filename)
        try:
            if os.path.isfile(file_path) or os.path.islink(file_path):
                os.unlink(file_path)
            elif os.path.isdir(file_path):
                shutil.rmtree(file_path)
        except Exception as e:
            print('Failed to delete %s. Reason: %s' % (file_path, e))

    with multiprocessing.Pool(processes=nthreads, initializer=start_process) as pool:
        pool.map(worker, [(
                i,
                p1, p2, lr_bin, start_seed, level, nthreads, count, team_size, profile_mode
        ) for i in range(count)])


if __name__ == "__main__":
    main()