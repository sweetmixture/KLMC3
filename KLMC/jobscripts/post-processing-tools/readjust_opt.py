import os
import re
from multiprocessing import Pool
from tqdm import tqdm

def process_file(i):
	old_filename = f"X{i}.gin"
	new_filename = f"A{i}.gin"
	
	# Rename the file
	try:
		os.rename(old_filename, new_filename)
	except:
		pass
	
	# Modify the file contents
	with open(new_filename, 'r+') as file:
		lines = file.readlines()
		updated_lines = []
		for line in lines:
			# resetting maxcyc
			if re.search(r'maxcyc opt\b', line):
				line = re.sub(r'(maxcyc opt)\b.*$', r'\1  2000', line.rstrip()) + '\n'	# Replace the line
			# turning off rfo
			if re.search(r'switch rfo 0\.00005\b', line):
				line = '#' + line  # Comment out the line by adding a '#' at the beginning
			updated_lines.append(line)
		
		file.seek(0)
		file.truncate()
		file.writelines(updated_lines)
	
	return i

if __name__ == "__main__":
	start_i = 0  # Starting value of i
	end_i = 1000  # Ending value of i
	num_processes = os.cpu_count() // 4  # Use a quarter of available CPU cores
	file_range = range(start_i, end_i + 1)	# Range of i values to process
	
	with Pool(num_processes) as pool, tqdm(total=len(file_range)) as pbar:
		for _ in pool.imap_unordered(process_file, file_range):
			pbar.update(1)
	
	print("All processes completed successfully.")
