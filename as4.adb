----------------------------csc410/prog4/as4.adb----------------------------
-- Author:	Matthew Bennett
-- Class:		CSC410 Burgess
-- Date:		10-05-04 							Modified: 10-17-04
-- Due:			10-12-04
-- Desc:		Assignment 4: DJIKSTRA'S STABILIZING ALGORITHM 
--
--	a nonproduction implementation of
--	DJIKSTRA's algorithm which describes
--	mutual exclusion, fairness, and deadlock avoidance
--  n processes (TASKS), and is self-correcting
--
--	BRIEF: (please read all of this)
--	n processes form a [directed] virtual ring topology, and
--		use an internal integer counter called flag to determine
--		which may go next. In general, a process may not proceed
--		unless its flag is not equal to its previous neighbor, except for process0
--
--	The algorithm is ASYMMETRIC. Process0 behaves differently than
--		processes [1..n] in several respects. Also, since the
--		asymmetrical behavior is neccessary for the algorithm to
--		continue, some other process MUST assume the behavior of
--		Process0 in case that process is killed or quits.
--		
--	The algorithm is self-stabilizing, so it should operate on a 
--		"dummy resource" for several iterations (up to 10) eahc time
--		the tasks are reordered. I will try to add this feature to
--		code soon.
--	!! MUTUAL EXCLUSION IS NOT GUARANTEED UNTIL THE TASKS STABILIZE !!
--
--	DJIKSTRA implemented as described in
--  	"Algorithms for Mutual Exclusion", M. Raynal
--  	MIT PRESS Cambridge, 1986 ISBN: 0-262-18119-3
--	with minor revisions
----------------------------------------------------------------

-- Refactorings  10-05-04: (denoted @FIX@)
--		(1) enumerated types {_PREV_, _SELF_, _NEXT_} instead of {-1, 0 1}
--		(2) message passing / reindezvouz instead of function calls
--		(3) slect instead of case
--		(4) linked list of processes instread of array
--		(5) randomly kill of processes including process #1, check for stabiliz.
--		(6) remove "magic" constants

----------------------------------------------------------------
-- dependencies

-- style note: the reasons to "with in" but not "use" packages are
--	(1) to avoid crowding of the namespace
--	(2) to be explicit where abstract data types and methods come from.
--	for instance, line 			randomPool : Ada.Numerics.Float_Random.Generator; 
--	is more explicit than		randomPool : Generator; 

WITH ADA.TEXT_IO; 							USE ADA.TEXT_IO;
WITH ADA.INTEGER_TEXT_IO; 			USE ADA.INTEGER_TEXT_IO;
WITH ADA.NUMERICS.FLOAT_RANDOM;	USE ADA.NUMERICS.FLOAT_RANDOM;
																--by request only
WITH ADA.CALENDAR; 							
-- (provides cast: natural -> time for input into delay)
WITH ADA.STRINGS; 						USE ADA.STRINGS;

----------------------------------------------------------------
----------------------------------------------------------------

PROCEDURE AS4 IS
	--globals are: RandomPool, constants scale_factor, max_tasks, SPACES
	randomPool : Ada.Numerics.Float_Random.Generator;
		-- yields a random Natural after seed

	MAX_TASKS : CONSTANT := 100;
		--global constant for mem allocation restriction
	SCALE_FACTOR : CONSTANT := 1.0;
		--used to increase "spread" of random delays
  SPACES : STRING(1..80) := (Others => ' ');
		--for convenience in string manipulations

	TYPE RX;		--outer, receiving task
		--this task contains djikstra's algorithm (djikstraTask),
		-- and also the rendezvouz entries for interprocess communication
		-- which is message passing, using entry calls.
		-- its primary job is to listen in case a DjistraTask of some other task
		-- tried to communicate with it, using the ENTRIES specified for the SELECT
		--	statement.
	TYPE RX_PTR IS ACCESS RX;
		--for our array, so we do not lose track

   ---- Begin RX_Task declaration and definition ----
	TASK TYPE RX IS --outer task spawns algorithm and then waits for a msg

		ENTRY	init ( --rendezvouz for initializing each task
				i			:	 Integer;		--process ID number. may NOT change if processes die
				self 	: RX_Ptr;			--pointer so the process can refer to itself
				prev 	: RX_Ptr;			--pointer to ring neighbor prev
				next 	: RX_Ptr;			--pointer to ring neighbor next
				selfFlag : Integer; --flag of self
				prevFlag : Integer; --flag of prev, for comparisons
				nextFlag : Integer; --flag of next, for comparisons
				n : Integer;				--total number of tasks
				lowID :	boolean			-- am I the chosen one?
			);

		ENTRY Receive(
				mesgType : Character; -- @FIX@ should be seprate rendezvous-es
				id : Integer; 			--	process id
				flag : Integer;			--	new flag value
				prev : RX_Ptr;			--	prev pointer
				next : RX_Ptr;			--	next pointer
				lowId : boolean			-- am I the new low?
			);

	END RX; --end specification

	TASK BODY RX IS

		-- "local globals" (specific to each task)
		procArray : array (0..2) of RX_Ptr;
		flagArray : array (0..2) of Integer := (Others => 0);
			--flagarray(0) is flag of prev process
			--flagarray(1) 	is flag of curr process
			--flagarray(2) 	is flag of next process
		--same holds for procArray

		myId 			: Integer;
		recId 		: Integer;
		num_count : Integer;

		suicide		: Boolean := false;
		 -- used to determine if the process' death has been requested
		lowestId : Boolean;
		 -- used to determine if special protocol is in order

		---- Begin AL_Task declaration and definition ----

	TASK TYPE djikstraTask IS
				 --gee, that was short
	END djikstraTask; --interior task, does the algorithm
		
	TASK BODY djikstraTask IS 
	BEGIN
	LOOP

		IF (lowestId) THEN
		-- Process 0, special protocol -- 
		LOOP
			EXIT WHEN flagArray(1) = flagArray(0);
		END LOOP;		--equivalent to wait loop
			
	--!!	IN  CRITICAL SECTION PROCESS[0]	!!-- 
    Put (myId, (80/myId -8));
		Put_line (" in CS");

		delay (Standard.Duration(	random(randomPool) * SCALE_FACTOR)	);
		-- note: callbacks, function calls, or code may be placed here
		-- in order to use this code in some production environment
		-- (in place of the delay)

		Put (myId, (80/myId-8));
		Put_line(" out CS");
  --!!	OUT CRITICAL SECTION	PROCESS[0] !!--             

		flagArray(1) := (flagArray(1) + 1) MOD num_count;
		procArray(2).Receive('U', myId, flagArray(1), null, null, FALSE);

		-- done Process0 protocol --

--=====================================================--

		ELSE --IF (NOT lowestID) THEN
    -- process[k NOT EQUAL 0] --
		LOOP
			EXIT WHEN (flagArray(1) /= flagArray(0));
		END LOOP;


	--!!	IN  CRITICAL SECTION PROCESS[K]	!!-- 
		Put (myId, (80/myId-8));
		Put_line (" in CS");

		delay (Standard.Duration(	random(randomPool) * SCALE_FACTOR)	);
		-- note: callbacks, function calls, or code may be placed here
		-- in order to use this code in some production environment
		-- (in place of the delay)

		Put (myId, (80/myId-8));
		Put_line (" out CS");
	--!!	OUT  CRITICAL SECTION PROCESS[K] 	!!-- 

		flagArray(1) := flagArray(0);
		procArray(2).Receive('U', myId, flagArray(1), null, null, FALSE);
	END IF;

END LOOP;
END djikstraTask;
---- End AL_Task declaration and definition ----

	TYPE Djik IS ACCESS djikstraTask;
	ptr : Djik; --pointer to interior task for algorithm from RX_TASK

	BEGIN
	-- Entry Point to initalize our id's from the main procedure
	ACCEPT
		init (
			i 	 			: IN Integer; --process number of self
			self 			: IN RX_Ptr;	--pointer to self
			prev 			: IN RX_Ptr;	--pointer to prev in ring
			next 			: IN RX_Ptr; 	--pointer to next in ring
			selfFlag 	: IN Integer;	--self's flag initializer
			prevFlag 	: IN Integer;	--flag of prev intial condition
			nextFlag 	: IN Integer;	--flag of next initial condition
			n				 	: IN Integer; --num tasks
			lowID 		: IN Boolean	--am i the chosen one?
		)
	DO
		procArray(1) := self;   -- "save my pointer to self"
		procArray(0) := prev;   -- "save my pointer to prev"
		procArray(2) := next;   -- "save my pointer to next, I'm in the list!"
		flagArray(1) := selfFlag; --initialize my flag
		flagArray(0) := prevFlag; --save my copy of prev's flag
		flagArray(2) := nextFlag; --save my copy of next's flag
		num_count := n;	--save number of processes
		myId := i;			--save self's process number
		lowestId := lowId;
  END init;

	ptr := new djikstraTask; --throw of an djikstra_TASK for each R(eceving X_TASK
         
	-- RX twiddles its thumbs and waits for messages.
	--	there should be a delay here, but I cant get the timing to work well
	--	without giving me significant lag problems
	LOOP 
	ACCEPT
	Receive (
		mesgType : Character; --@FIX@ it!
		id : Integer;
		flag : Integer; 
		prev : RX_Ptr;
		next : RX_Ptr;
		lowId: boolean
	)
	DO
    CASE mesgType IS

			WHEN 'U' => -- update message
			BEGIN
				Put (myId, (1+4*myId));
				Put (",");
				Put (id,0);
				Put_line(" Update");
					-- As we always receive update messages from the prev neighbour
					-- we simply change our local copy of prev's new flag
				flagArray(0) := flag;
			END;
			
			WHEN 'N' => -- newnode message
			BEGIN
				Put (myId, (1+4*myId));
				Put (",");
				Put (id,0);
				Put_line (" Neighbour"); 

				IF (prev = null)
				THEN -- Someone is updating our next neighbour
					procArray( 2) := next;
					flagArray( 2) := flag;
				ELSE -- Someone is updating our prev neighbour
					procArray(0) := prev;
					flagArray(0) := flag;
					lowestID := lowID;
				END IF;
			END;

			WHEN 'R' => -- remove msg -- this TASK is doomed!!
			BEGIN
				Put (myId, (1+4*myId));
				Put (",");
				Put (id,0);
				Put_line (" RIP");
				procArray(0).Receive('N', myId, flagArray(2),null,procArray(2),FALSE); 					-- Tell my prev who my next was
					-- Also, if dying, we pass true as the lowest id to the next,
					-- as it is our next lowest, if we were the lowest already
				IF (lowestID)
				THEN
				procArray(2).Receive('N',myId,flagArray(0), procArray(0), null,TRUE); 						-- Tell my next who my prev was, and crown it the new low
				ELSE
				procArray(2).Receive('N',myId,flagArray(0), procArray(0),null,FALSE); 						-- Tell my next who my prev was
				END IF;
				suicide := true; -- kill thyself, as requested by a higher power!
			END;
			WHEN Others => null; --needed for {case/select} default path (do nothing)
		END CASE;
		END Receive;      

	EXIT WHEN suicide;

END LOOP;
END RX;
---- End RX_Task declaration and definition ----

PROCEDURE Driver IS	--seperate out variables to avoid excess globals
	numtasks_user, seed_user : Integer; --user defined at runtime by std. in
  ptrArray : array (0..MAX_TASKS) of RX_Ptr; --to keep up with each task spun
	specialProtocol : boolean; --temp var used for which djistra protocol to use

BEGIN
	put("# random seed:       ");
	get(seed_user); --to ensure a significantly random series, a seed is needed
									-- to generate pseudo-random numbers
 	Ada.Numerics.Float_Random.Reset(randomPool,seed_user);
		--seed the random number pool

	--sanity checked on the input
	LOOP
		put("# tasks[1-50]:       ");
		get(numTasks_user);
		--sanity checked input
		EXIT WHEN (numTasks_user > 0 AND numTasks_user <= MAX_TASKS);
	END LOOP;

	FOR task_counter IN 0 .. (numTasks_user - 1)
  LOOP
		ptrArray(task_counter) := new RX;
  END LOOP; --spin out all our processes
						--seperate, bc we'll want pointers to next process at initialization
	FOR task_counter IN 0 .. (numTasks_user - 1)
  LOOP
		IF TASK_COUNTER = 0 THEN specialProtocol := TRUE;
		ELSE specialProtocol := FALSE; END IF;

		ptrArray(task_counter).init(
			task_counter,						--this will be the process' id number
			ptrArray(task_counter), --this is the process' pointer to itself
			ptrArray((task_counter - 1) MOD numTasks_user), --process' pointer to prev
			ptrArray((task_counter + 1) MOD numTasks_user), --process' pointer to next
			task_counter, 					-- process' initial flag ( so flag(n)=n )
															-- in order to demonstrate stabilization
    	((task_counter - 1) MOD numTasks_user),
				--process' initial prev flag ( so flag(_PREV)=n-1 )
			((task_counter + 1) MOD numTasks_user),
				--process' initial next flag ( so flag(_NEXT)=n+1 )
    	numTasks_user,					--number of processes, which is constant
			specialProtocol					--flag for asymmetry in the algorithm
		);
  END LOOP; --initialize out all our processes

	FOR kill_index IN REVERSE 0 .. (numTasks_user - 1)
	LOOP
    delay (600.0);
		Put_Line ("Going to kill random process ... ");
		ptrArray(kill_index).Receive('R', 0, 0, null, null, FALSE);
  END LOOP; --kill out all our processes

END Driver;
-- main exec for AS4.adb:
BEGIN	Driver; END AS4;
