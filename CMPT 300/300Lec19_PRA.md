10 Pages | Lec 19 - Tues - July 21
- Section 10.4 Page Replacement
### Intro (Page 2 - 4)
Over-allocating of memory manifests itself as flows
- while a process is executing, a page fault occurs
- the OS determines where the desired page is residing on disk 
  - but then finds that there are **NO Free Frames**
  - all memory is in use
  
![](imgs/no_free_frame.jpeg)

Three options if no free frames to put a new page into
1. Terminate the Process
- However, demand paging is the OS attempt to improve the utilization and throughput
- User should not be aware that their processes are running on a paged system
  - paging should be logically transparent to the user => NOT the best choice
2. Swap out the process, freeing all its frames and reducing the level of multiprogramming
- However, swapping is no longer used by most OS
  - Due to the **overhead of copying ENTIRE processes** between memory and swap space
3.(BEST) Combine Page Replacement with swapping

Page Replacement Approach:
- If NO Frame is Free
  - we find one that is not currently being used and 
  - free a frame by writing its contents 
    - to swap space and changing the page table
    - to indicate the page is no longer in memory
- Use the freed frame to hold the page 
  - for which the process faulted
  
Modify the page-fault service routine to inlude page replacement (Quiz 5 Q6)
### The FIFO PRA (Page 5 - 7)t 
The simplest algorithm a
### The Optimal Page Replacement Algorithm (Page 8 - 10)
