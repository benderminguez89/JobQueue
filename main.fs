open System

let AssertEquality inputTuple =
  match inputTuple with
  |(a,b) when a=b -> printfn "Test Success"
  |_ -> printfn "Test Fail"

(********************************************************************************************************************
Step 1:
Sample random number list: 3,4,6,1,9,2,5,6
let create_job_list jobCount resultLis =
___ 
Print out the whole list for easier debugging. This function uses tail recursion. At each function call, add a new random number into your resultLis.
*********************************************************************************************************************)

let rec create_job_list jobCount resultLis =
    let rand = new System.Random()
    let job = rand.Next(1,20)
    match jobCount with
    | 0 -> resultLis
    | _ -> create_job_list (jobCount - 1) (job::resultLis)

let jobList = create_job_list 10 []
printfn "%A" jobList

(********************************************************************************************************************
Step 2:
Implement a Queue type that has enqueue and dequeue functions. You can reference the stack calculator to make a Queue. Call the enqueue and dequeue a few times to make sure it is working. Print out some testing information for debugging.
********************************************************************************************************************)

type Queue = QueueContents of int list

let enqueue x (QueueContents contents)= 
    QueueContents (contents@[x])
     

let dequeue (QueueContents contents) = 
    match contents with 
    |head::tail -> 
      let newQueue = QueueContents tail
      (newQueue , head)
    |[] -> failwith "Job Queue is empty!"

let emptyQueue = QueueContents []
let queueWith1test = enqueue 1 emptyQueue 
let queueWith2test = enqueue 2 queueWith1test 
let dequeueTest = dequeue queueWith2test

AssertEquality (queueWith2test, QueueContents [1;2])
printfn "%A" queueWith2test
printfn "%A" dequeueTest

(*******************************************************************************************************************
Step 3:
Use this data structure to make a job queue simulator following below example:
Go through the random number list with tail recursion:
 --- if the current element is an odd number A, enqueue the new task with a printout notice “Added task No. A into the job queue”. 
--- if the current element is an even number B, dequeue a task from the Queue and print out which task is done with format “Do job No. A now”. 
---if the queue is empty, and you get an even number, print out a notice “No more job to do right now.”. 

With the random number list 3,4,6,1,9,2,5,6, the program behaves like below:
Head=3, odd number, enqueue 
-> queue has 3 in it. Print out “Added task No.3 into the job queue”. Remaining list 4,6,1, 9, 2,5,6
Head=4, even number, dequeue
	-> Dequeue 3. Print out “Do job No.3 now”. Queue is empty. Remaining list 6,1, 9, 2,5,6
Head=6, even number, dequeue
	->Print out “No more job to do right now”. Remaining list 1, 9, 2,5,6
Head=1, odd number, enqueue
	-> queue has 1 in it. Print out “Added task No.1 into the job queue”. Remaining list 9, 2,5,6
Head=9, even number, enqueue
	->queue has 1,9 in it…
*********************************************************************************************************************)
let rec jobQueue jobNums currJobs= 
    match jobNums with
    |[] -> jobQueue
    | head::tail when head%2=0 -> jobQueue tail (dequeue currJobs)
    | head::tail when head%2=1 -> jobQueue tail (enqueue currJobs head)

let Jobs = jobQueue jobList []
printfn "%A" Jobs