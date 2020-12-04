#!/usr/bin/env python3

import click
import subprocess
import json
import os
import time
import logging
import traceback
import multiprocessing
import glob

log = logging.getLogger()


@click.group()
def main():
	pass


def analyze(base_path):
	cwd = os.getcwd()

	full_path = cwd + "/" + base_path + ""
	os.chdir(full_path)

	while True:
		process(full_path)
		time.sleep(10)


def process(full_path):
	won_new = 0
	won_old = 0

	idx = 0
	games = 0
	# for sw in ['_0', '_1']:
	for sw in ['_', '_swap_']:
		swap = idx == 1
		idx += 1

		# print("SW: " + sw + "\n\n")

		for file in glob.glob(f"{sw}result*.txt"):
			fp = full_path + file
			sz = int(os.path.getsize(fp))
			if sz == 0:
				# print("passing file " + fp)
				continue

			games += 1
			# print(fp)

			with open(fp, "r") as js:
				j = json.load(js)
				#print(j['results'])
				res = j['results']
				if swap:
					(res[0],res[1])=(res[1], res[0])
					(res[2],res[3])=(res[3], res[2])

				max_value = max(res)
				max_index = j['results'].index(max_value)
				if max_index % 2 == 0:
					won_new = won_new + 1
				else:
					won_old = won_old + 1

	print(won_new, " : ", won_old, "\t\t", "\t\tGames: ", games, "\t", flush=True)


@main.command()
@click.option('--path', type=str, required=True, default="tmp\\")
def run(path):
	if not os.path.exists(path):
		print(dir, " not found.")
		return

	print("analyzing " + path)

	analyze(path)


if __name__ == "__main__":
	main()
