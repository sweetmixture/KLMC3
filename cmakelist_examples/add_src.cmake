# Initialize the SRC variable with common files
set(SRC
    taskfarm.c
    taskfarm_def.c
    read_input.c
    master_worker_ready_input.c
    timer.c
    print_message.c
    error.c
    file.c
)

# Conditionally add additional files based on some condition
if(CONDITION_TRUE)
    list(APPEND SRC
        new_file1.c
        new_file2.c
        new_folder/file3.c
    )
endif()


# BY OVERWRITING

# Initialize SRC with common files
set(SRC
    taskfarm.c
    taskfarm_def.c
    read_input.c
    master_worker_ready_input.c
    timer.c
    print_message.c
    error.c
    file.c
)

# Conditionally overwrite SRC based on some condition
if(CONDITION_TRUE)
    # Add additional files to SRC
    set(SRC
        ${SRC}
        new_folder/file1.c
        another_folder/file2.c
        # Add more files as needed
    )
endif()
