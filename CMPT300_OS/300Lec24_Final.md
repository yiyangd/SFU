Augest 6 | Thursday | 37 Pages
Readings:  
Chapter 13: File System Interface - section 13.4 - Protection  
Chapter 14: File System Implementation - section 14.5 - Free-Space Management


### 1. File System Reliability (Page 2 - 3 | Sec 13.4 - Protection)

### 2. File Access Management (Page 4 - 8 )
#### 2.1 UNIX File Permission (Page 5)
#### 2.2 Windows

### 3. File Allocation: Free Space Management (Page 10 | Sec 14.5)
#### To implement FSL
#### 3.1 Bit Vector (Page12) 
#### 3.2 Linked List (Page13)
#### 3.2.1 Improvement (Page 14 - 18)
 
### 4. UNIX: The Primitive Pieces
Superblock: The Unix File System volume control block (per volume) that contains volume details
- helps locate everything
- 

Inode: a data structure for storing file system metadata - with pointers to its data (Sec11.8.6)

Maintains global file system information, such as
- the number of blocks in the volume
- the size of the blocks 
- a free-block count 
- free-block pointers
- a free-FCB count and FCB pointers

![Superblock + Inodes + data blocks and directories](imgs/superblock.jpeg)

#### 5. Creating a File (Sec 13.1.2)
