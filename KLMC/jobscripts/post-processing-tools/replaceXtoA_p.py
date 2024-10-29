import os
from multiprocessing import Pool
from tqdm import tqdm

def process_file(i):
    old_filename = f"X{i}.gin"
    new_filename = f"A{i}.gin"
    
    # Rename the file
    os.rename(old_filename, new_filename)
    
    # Modify the file contents
    with open(new_filename, 'r+') as file:
        lines = file.readlines()
        updated_lines = [line for line in lines if 'output xyz' not in line]
        file.seek(0)
        file.truncate()
        file.writelines(updated_lines)
    
    return i

if __name__ == "__main__":
    start_i = 0  # Starting value of i
    end_i = 199999  # Ending value of i
    num_processes = os.cpu_count() // 4  # Use a quarter of available CPU cores
    file_range = range(start_i, end_i + 1)  # Range of i values to process
    
    with Pool(num_processes) as pool, tqdm(total=len(file_range)) as pbar:
        for _ in pool.imap_unordered(process_file, file_range):
            pbar.update(1)
    
    print("All processes completed successfully.")

