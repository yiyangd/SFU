## July 9 Lecture 16 Thursday | 31 Pages
- Section 9.2 - Contiguous Memory Allocation
- Section 9.3 - Paging
- Section 9.5 - Swapping Page 2 - 8


### 1. Swapping （Page 2 | Section 9.5)
Process instructions and the data they operate on must 【be in memory】 to be execute 
- However, a process (or a portion of a process) can be `swapped` temporarily 【out of memory】 to a `backing store`(e.g. Disk) and then brought 【back into memory】for continued execution
  - Current memory contents written to a **backing store** (disk)
  - **Memory Image** for the next user process read in
  - Ready Queue contains processes whose memory images are on disk (and ready to run)

![Swapping](imgs/Swapping.jpeg)

The Backing Store Candidates:
- fast secondary storage (i.e. disk)
- large enough to accommodate whatever parts of processes need to be stored and retrieved
- provide direct access to these **memory images**

The Swapping Candidates:
- Idle or mostly idle processes
- Inactive

Context Switch (Section 3.2.3 | Page 3-4)
- Def: Switching the CPU core to another process requires performing a state save of the current process and a state restore of a different process.
- This Task is time-consuming
  - Dependent Upon the device performance
  - If we used a scheduling scheme like Round Robin, we would want a VERY large **time quantum**
  
We need to set up two `buffers`: "incoming" and "outgoing"
- At context Switch time we do two memory-to-memory copies
- During Application Execution, we do two disk-to-memory copies to update the buffers 
  - requires a DMA device
#### Advantage of swapping
Swapping makes it possible for the **total physical address space** of all processes to exceed the **real physical memory** of the system (oversubscribed)
- ==> increasing the degree of multiprogramming in a system

#### Swapping with Partitions (Page 5 - 8)
Swapping with multiple programs(buffers)
- A fence is inadequate

Because the amount of time required to move entire processes between memory and the backing store is prohibitive (望而却步)
- Do some copying at context switch time (just more overhead)
  - overhead: the processing time required by a device prior to the execution of a command
- Need to `protect access` both before and beyond the User Program（Lec15）
  - Upper/Lower Bound
  - Base/Limit Registers
    - The relocation register contains the value of the smallest physical address
    - The limit Register contains the range of logical addresses
    - Each logical address must fall within the range specified by the limit register
    - The MMU maps the logical address `dynamically` by adding the value in the base register
      - This mapped address is sent to memory

Part of the context switching is the reloading of these registers with the correct values by the dispatcher(Sec 9.2.1)
- when CPU scheduler selects a process for execution
- Now, swapping can be done outside of the user space, W/O copying

**Most Significant**: Swapping can be **eliminated** in some cases:
- Processes can stay in memory between CPU Bursts

### 2. Contiguous Memory Allocation ( Page 9 - 16 | Section 9.2)
The memory is usually divided into a set of fixed partitions:
- one for the OS
- the rest for the various user processes 

Each Partition can be scheduled separately
- Possibly a separate ready queue for each memory partition

**Bounds are set** just before and after the partition for the running job
- A Process cannot span partition boundaries
- A partition cannot be shared
  - when a process is done, another approprivately sized job will take its place

#### 2.1 Memory Allocation (Sec 9.2.2 | Page 10)
The simplest method is to assign processes to variably sized partitions in memory, 
- where each partition may contain exactly one process.

The OS keeps track of (a table to indicate) which memory is in use and which are available 
- Initially, all memory is available for user processes and is considered one **large block** of available memory, `a hole`
- Eventially, memory contains **a set of holes** of various sizes （大大小小的空余空间）
- We could add swapping done separately for each partition 
  - e.g. When 6k job begins an I/O burst, we could swap it and put another job in.

(Fixed Number of Tasks)What if a 1.5K process in the 2K spot asks for another 1K of memory?
- 1. abort the process
- 2. deny the request
- 3. swap it out and put it on the queue for the 6K partition
  
Region size selection is difficult (**For a Fixed Number of Tasks**)

#### 2.2 Dynamic Storage Allocation Procedure (Page 13 - 19)
For a variable number of Tasks
- MVT - "Multiple, contiguous variable partition allocation"

When a process arrives and needs memory,
- the system searches the set for a **hole** that is large enough for this process
- If the hole is too big, it is split into two parts
  - one part is allocated to the arriving process
  - the other is returned to the set of holes.

When a process terminates,
- it releases its block of memory, which is then placed back in **the set of holes** 
- If the new hole is adjacent to other holes, they are merged to form one larger contiguous block.

---
- Dynamic Storage allocation Problem: how to satisfy a request of `size n` from a list of free holes?
---
#### 3 Solutions (Page 18 - 19)
First Fit
Best Fit
Worst Fit

Example 

#### 2.2 Fragmentation (Section 9.2.3 | Page 11 - 12)
Internal Fragmentation: memory i
#### Compaction (Page 20 - 22, Sec 9.2.3)
One solution to the problem of external fragmentation.
- Goal: Shuffle the memory contents so as to place all free memory together in one **large block**



### 3. Paging (Page 23 - Page 31 | Section 9.3)
