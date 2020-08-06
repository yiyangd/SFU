## July 14 | Lecture 17 | 10 pages
- 9.3.2:
- 9.3.3: Protect
- 9.3.4: Shared Page
- Modern Operating Systems Section 3.7: Segmentation

### 1. Page Table Implementation (Lec16_Page 29 - 31 | Section 9.3.2: Hardware Support)
The page table for each process is kept in its PCB (Process Control Block: to represent each process in the OS | Section3.1.3 )
- There is also a hardware register that has a pointer(like the instruction pointer) to the page table for the currently executing process
- When CPU scheduler selects a process for execution,
  - it must reload the user registers
  - and the appropriate hardware page-table values from the **stored user page table**

Where do we store the page table? 
- if the page table is reasonably small (256 entries):
  - the page table is implemented as a set of dedicated high-speed **hardware registers**
    - makes the page-address translation efficient!
    - but increases context-switch time, during which each one of these registers must be exchanged
  
- Larger page tables (2<sup>20</sup> entries)
  - the page table is kept in main memory
  - a **page-table base register**(PTBR) points to the page table
  - cache called TLB (Translation Look-Aside Buffer)
  
  
  ### 2. Segmentation (分段：Page2 - 4 & 8 - 10 | <Modern OS 4th> Section 3.7）
  
  #### 2.1 Fragmentation in Segmentation (Page 8）
  #### 2.2 Paged Segmentation (MULTICS) (Section 3.7.2)
  ### 3. Protection ( Slides page 5 | Section 9.3.3)
  ### 4. Shared Page ( slides page 6 - 7)
  
  #### TOGO：300Lec18 - Virtual Memory （ Section 10.1, 10.2 - VM, Demanding Paging）
