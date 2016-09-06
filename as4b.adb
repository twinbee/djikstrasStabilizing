----------------------------csc410/prog4/as4.adb----------------------------
-- Author:	Matthew Bennett
-- Class:		CSC410 Burgess
-- Date:		10-05-04 							Modified: 10-22-04
-- Due:			10-12-04
-- Desc:		Assignment 4: DJIKSTRA'S STABILIZING ALGORITHM 
--
--	a nonproduction implementation of
--	DJIKSTRA's algorithm which describes
--	mutual exclusion, fairness, and deadlock avoidance
--  n processes (TASKS), and IS self-correcting
--
--	BRIEF: (please read all of this)
--	n processes form a [directed] virtual ring topology, and
--		use an internal integer counter called flag to determine
--		which may go next. IN general, a process may not proceed
--		unless its flag IS not equal to its previous neighbor, except FOR process0
--
--	The algorithm IS ASYMMETRIC. Process0 behaves differently than
--		processes [1..n] IN several respects. Also, since the
--		asymmetrical behavior IS neccessary FOR the algorithm to
--		continue, some other process MUST assume the behavior of
--		Process0 IN case that process IS killed or quits.
--		
--	The algorithm IS self-stabilizing, so it should operate on a 
--		"dummy resource" FOR several iterations (up to 10) eahc time
--		the tasks are reordered. id will try to add this feature to
--		code soon.
--	!! MUTUAL EXCLUSION IS NOT GUARANTEED UNTIL THE TASKS STABILIZE !!
--
--	DJIKSTRA implemented as described IN
--  	"Algorithms FOR Mutual Exclusion", M. Raynal
--  	MIT PRESS Cambridge, 1986 ISBN: 0-262-18119-3
--	with minor revisions
----------------------------------------------------------------

-- dependencies

-- style note: the reasons to "with IN" but not "use" packages are
--	(1) to avoid crowding of the namespace
--	(2) to be explicit where abstract data types and methods come from.
--	FOR instance, line 			randomPool : Ada.Numerics.Float_Random.Generator; 
--	IS more explicit than		randomPool : Generator; 

WITH ADA.TEXT_IO; 							USE ADA.TEXT_IO;
WITH ADA.INTEGER_TEXT_IO; 			USE ADA.INTEGER_TEXT_IO;
WITH ADA.NUMERICS.FLOAT_RANDOM;	USE ADA.NUMERICS.FLOAT_RANDOM;
																--by request only
WITH ADA.CALENDAR; 							
-- (provides cast: natural -> time FOR input into delay)
WITH ADA.STRINGS; 						USE ADA.STRINGS;

----------------------------------------------------------------
----------------------------------------------------------------

PROCEDURE AS4B IS

 --globals are: randompool,	MAX_TASKS, SCALE_FACTOR, SPACES
	randomPool : ada.numerics.float_random.Generator;

	MAX_TASKS : CONSTANT := 100;
		--global constant for mem allocation restriction
	SCALE_FACTOR : CONSTANT := 1.0;
		--used to increase "spread" of random delays
  SPACES : STRING(1..80) := (Others => ' ');
		--for convenience in string manipulations

	TYPE RX;
	TYPE RX_Ptr IS ACCESS RX;
	--so that we 

   ---- BEGIN RX declaration and definition ----
TASK TYPE RX IS

	ENTRY initvars (
		id : Integer;
		this : RX_Ptr;
		left : RX_Ptr;
		right : RX_Ptr;
		thisFlag : Integer;
		leftFlag : Integer;
		rightFlag : Integer;
		numTasks_user : Integer;
		lowId : Boolean
	);


	ENTRY Receive (
		mesgType : Character;
		id : Integer;
		flag : Integer; 
		left : RX_Ptr;
		right : RX_Ptr;
		lowId : Boolean
	);

END RX;


TASK BODY RX IS

	procArray : array (0..2) of RX_Ptr;
	flagArray : array (0..2) of Integer := (Others => 0);
	myId : Integer;
	num_count : Integer;
	lowestId : Boolean;

	ShouldIDie : Boolean := FALSE;
      
      ---- BEGIN djikstra declaration and definition ----
      
TASK TYPE djikstra IS
 --gee, that was short
END djikstra;

TASK BODY djikstra IS 
BEGIN
LOOP
	IF (lowestId) THEN
	-- Protocol 0
		LOOP null;
			EXIT WHEN flagArray(1) = flagArray(0);
		END LOOP;

	-- BEGIN Critical Section -- 
		Put (myId, (1+(4*myId))); Put (" IN CS"); NEW_Line;
		delay (Standard.Duration(random(randomPool) * 4.0));
		Put (myId, (1+(4*myId))); Put (" out CS"); NEW_Line;
	-- END Critical Section --             
  
	flagArray(1) := (flagArray(1) + 1) mod num_count;
	procArray(2).Receive('U', myId, flagArray(1), null, null, FALSE);

	ELSE
  -- Protocol K NOT 0
	LOOP null;
		EXIT WHEN (flagArray(1) /= flagArray(0));
	END LOOP;

	-- BEGIN Critical Section
	Put (myId, (1+(4*myId))); Put (" IN CS"); NEW_Line;
	delay (Standard.Duration(random(randomPool) * 4.0));
	Put (myId, (1+(4*myId))); Put (" out CS"); NEW_Line;
	-- END Critical Section -- 

	flagArray(1) := flagArray(0);
	procArray(2).Receive('U', myId, flagArray(1), null, null, FALSE);
	END IF;
END LOOP;
         
END djikstra;

	TYPE djik_ptr IS ACCESS djikstra;
	ptr : djik_Ptr;
--so that we can spin off a copy of djikstra's algorithm for each listener task


BEGIN   
-- ENTRY Point to initalize our id's from the AS4 PROCEDURE
ACCEPT initvars (id : Integer; this : RX_Ptr; left : RX_Ptr; right : RX_Ptr; 
            thisFlag : Integer; leftFlag : Integer; rightFlag : Integer;
            numTasks_user : Integer; lowId : Boolean) do
         procArray(1) := this;   
         procArray(0) := left;
         procArray(2) := right;
         flagArray(1) := thisFlag;
         flagArray(0) := leftFlag;
         flagArray(2) := rightFlag;
         num_count := numTasks_user;
         myId := id;
         lowestId := lowId;
      END initvars;
      ptr := NEW djikstra;   
         
      -- Start our message handling LOOP waiting FOR messages to arrive -- 
LOOP
SELECT

	ACCEPT Receive (
		mesgType : Character;
		id : Integer;
		flag : Integer;
		left : RX_Ptr;
		right : RX_Ptr;
		lowId : Boolean
	) DO
            case mesgType IS
               WHEN 'U' => 
                  BEGIN
			Put(id, (80 - 10 ) );
			Put_Line(" update");
                     NEW_Line;   
                     -- As we always receive update messages from the left neighbour
                     -- we simply change our local copy of left's NEW flag
                     flagArray(0) := flag;
                  END;
               WHEN 'N' => 
                  BEGIN
			Put(id, (80 - 10 ) );
			Put_Line(" neighbor");
                     NEW_Line;   
                     IF (left = null) THEN -- Someone IS updating our right neighbour
                        procArray(2) := right;
                        flagArray(2) := flag;
                     ELSE -- Someone IS updating our left neighbour
                        procArray(0) := left;
                        flagArray(0) := flag;
                        lowestId := lowId;   
                     END IF;
                  END;
               WHEN 'R' => 
                  BEGIN
			Put(id, (80 - 10 ) );
			Put_Line(" dying...");
                     procArray(0).Receive('N', myId, flagArray(2), 
                        null, procArray(2), FALSE); -- Tell my left who my right was
                     -- Also, IF dying, we pass TRUE as the lowest id to the right,
                     -- as it IS our next lowest, IF we were the lowest already
                     IF (lowestId) THEN
                        procArray(2).Receive('N', myId, flagArray(0), procArray(0),
                            null, TRUE); -- Tell my right who my left was
                     ELSE
                        procArray(2).Receive('N', myId, flagArray(0), procArray(0),
                            null, FALSE); -- Tell my right who my left was
                     END IF;   
                     ShouldIDie := TRUE; -- Set flag to drop out of LOOP now
                  END;
               WHEN Others => null;
            END case;
         END Receive;      
      EXIT WHEN ShouldIDie = TRUE;
		END SELECT;
	END LOOP;
END RX;
   ---- END RX declaration and definition ----

PROCEDURE Driver IS

	numtasks_user : Integer;
	seed_user : Integer; --user defined at runtime by std. input
 
	ptrArray : array (0..MAX_TASKS-1) of RX_Ptr;

	killId		:	Integer;									--temp to store which process to kill
  RIPArray	: array (0..MAX_TASKS-1) of Boolean := (Others => FALSE);
	--keep track of which processes have been killed


BEGIN
	put("# random seed:       ");
	get(seed_user); --to ensure a significantly random series, a seed IS needed
									-- to generate pseudo-random numbers
 	Ada.Numerics.Float_Random.Reset(randomPool,seed_user);
		--seed the random number pool

	LOOP
		put("# tasks[1-50]:       ");
		Get (numTasks_user);
		EXIT WHEN (0 < numTasks_user) AND (numTasks_user <= MAX_TASKS);
	END LOOP;
    
	-- Create each of our tasks
	FOR task_count IN 0 .. numTasks_user-1
	LOOP
		ptrArray(task_count) := NEW RX;
	END LOOP;            


	FOR task_count IN 0 .. numTasks_user-1
	LOOP
		ptrArray(task_count).initvars(task_count, ptrArray(task_count), 
		ptrArray((task_count - 1) mod numTasks_user), 
		ptrArray((task_count + 1) mod numTasks_user), task_count, 
		((task_count - 1) mod numTasks_user),((task_count + 1) mod numTasks_user),
		numTasks_user, FALSE);
	END LOOP;

	delay (120.0);

--start randomly killing off processes   
FOR kill_index IN REVERSE 0 .. (numTasks_user - 1)
LOOP
delay (3.0);
 put ("Going to kill random process ... process "); 
     -- Delay 60 seconds THEN kill off a process -- 
	LOOP
		killID := Integer (random(randomPool)) mod numTasks_user;
		EXIT WHEN NOT RIParray(killID);
	END LOOP;
	RIParray(killID) := TRUE;
	Put(killID, 0); Put_line("");
	ptrArray(killID).Receive('R', 0, 0, null, null, FALSE);
END LOOP; --kill out all our processes

END Driver;

BEGIN Driver; null; END AS4B;
