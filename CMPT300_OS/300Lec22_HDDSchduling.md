Lecture #22: Thursday, July 30  | 10 Pages
Readings: Chapter 11: Mass Storage Structure - section 11.2


### 1. Algorithm
### 1.1 FCFS Scheduling (Page 3)
---
#### (Exercise 11.12) 
Why SSDs often use a FCFS Scheduling algorithm?  
**Answer:** Because SSDs do not have moving parts and therefore performance is insensitive to issues such as seek time and rotational latency. 
- Therefore, a simple FCFS policy will suffice.


---
### 1.2 SCAN Scheduling (Page 5)
### 1.3 C-SCAN Scheduling (Page 7)
### 2. Disk Scheduling Optimization (Page 8 - 10)

Sector Queuing, or SLTF (“shortest latency time first”)
- A rotational latency optimization

---
#### (Practice Exercises 11.3)
Why is rotational latency usually not considered in disk schduling? How would modify SSTF, SCAN, and C-SCAN to include **latency optimization**?  

Answer: Most disks do not export their rotational position information to the host. 
- Even if they did, the time for this information to reach the scheduler would be subject to imprecision and the time consumed by the scheduler is variable, so the rotational position information would become incorrect. 
- Further, the disk requests are usually given in terms of logical block numbers, and the mapping between logical blocks and physical locations is very complex.
---
