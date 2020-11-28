## CMPT300 Summer2020 Assignment3
---
翻书：unix网络编程
371书：UDP vs TCP
### IPC and Concurrency 
---
### Goals & Things to learn:
- Use `UNIX UDP IPC` (txtbook: 
- Programming Concurrent Processes
- How to program using the `client/server model`
- write a `multi-threaded program` using `pthreads` (which is representative of most other `threads packages`)
- solution to the critical section problem between threads.
---

### Overview
- Create a `"chat"-like facility` that enables someone at one terminal (or Xterm) to communicate with someone at another terminal
- This program will be called `"s-talk"` for simple-talk
- To initiate an s-talk session two users will first agree on two things:
  - the machine that each will be running on
  - the `port number` each will use
  
  
  
---
### Marking & Submission
Testing: 
- Pipe
```shell
cat someTestData.txt | ./s-talk <args> <go> <here>
```
- Pipe
```shell
./s-talk <args> <go> <here> >> someOutputData.txt
```
  - 
- UDP packets will not be tested, so can use whatever `message structure` 
