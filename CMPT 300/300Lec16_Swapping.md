## July 9 Lecture 16 Thursday | 31 Pages
- Section 9.2 - Contiguous Memory Allocation
- Section 9.3 - Paging
- Section 9.5 - Swapping


### 1. Swapping （Page 1-8 Section 9.5)
Process instructions and the data they operate on must 【be in memory】 to be execute 
- However, a process (or a portion of a process) can be `swapped` temporarily 【out of memory】 to a `backing store`(e.g. Disk) and then brought 【back into memory】for continued execution
  - Current memory contents written to a **backing store**
  - **Memory Image** for the next user process read in
  - Ready Queue contains processes whose memory images are on disk (and ready to run)

![Swapping](imgs/swapping.jpeg)

The backing store Candidates:
- fast secondary storage
- large enough to accommodate whatever parts of processes need to be stored and retrieved
- provide direct access to these **memory images**

The Swapping Candidates:
- Idle or mostly idle processes
- Inactive

#### Advantage of swapping (Page 2)
Swapping makes it possible for the **total physical address space** of all processes to exceed the **real physical memory** of the system (oversubscribed)
- ==> increasing the degree of multiprogramming in a system
#### Swapping with Partitions
Swapping with multiple
**Most Significant**: Swapping can be eliminated in some cases:
- Processes can stay in memory between CPU Bursts
### 2. Multiprogramming ( Page 9 - 16 | Section 9.2)
The memory is usually divided into a set of fixed partitions:
- one for the OS
- the rest for the various user processes
Each Partition can be scheduled separately
- Possibly a 
### 3. General Dynamic Storage Allocation Problem ( Page 17 - 19)
### 4. Compaction (Page 20, Sec 9.2.3
One solution to the problem of external fragmentation.
- Goal: Shuffle the memory contents so as to place all free memory together in one **large block**

### 5. Paging (Page 23 - Page 31 | Section 9.3)
