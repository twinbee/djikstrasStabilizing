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
--		the tasks are reordered. id will try to add this feature to
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

procedure Main is
   -- Globals --
   G : Generator;         -- Float Random Number Generator
   -- End Globals -- 

   type RX_Task;
   type RX_Ptr is access RX_Task;
   ---- Begin RX_Task declaration and definition ----
   task type RX_Task is
      entry Start (id : Integer; this : RX_Ptr; left : RX_Ptr; right : RX_Ptr;
                     thisFlag : Integer; leftFlag : Integer; rightFlag : Integer;
                     numProcesses : Integer; lowId : Boolean);
      entry Receive (mesgType : Character; id : Integer; flag : Integer; 
                     left : RX_Ptr; right : RX_Ptr; lowId : Boolean);
   end RX_Task;
   task body RX_Task is
      procArray : array (0..2) of RX_Ptr;
      flagArray : array (0..2) of Integer := (Others => 0);
      myId,recId,num_count : Integer;
      ShouldIDie : Boolean := false;
      lowestId : Boolean;
      
      ---- Begin AL_Task declaration and definition ----
      
      task type AL_Task is end AL_Task;
      task body AL_Task is 
      begin
      loop
         if (lowestId) then
				-- Protocol 0 -- 
            loop exit when flagArray(1) = flagArray(0); end loop;
            -- Begin Critical Section -- 
            Put (myId, (1+(4*myId))); Put (" in CS"); New_Line;
            Delay (Standard.Duration(random(g) * 4.0));
            Put (myId, (1+(4*myId))); Put (" out CS"); New_Line;
            -- End Critical Section --             
            flagArray(1) := (flagArray(1) + 1) mod num_count;
            procArray(2).Receive('U', myId, flagArray(1), null, null, false);
         else
            -- Protocol id --
            loop exit when (flagArray(1) /= flagArray(0)); end loop;
            -- Begin Critical Section --
            Put (myId, (1+(4*myId))); Put (" in CS"); New_Line;
				Delay (Standard.Duration(random(g) * 4.0));
				Put (myId, (1+(4*myId))); Put (" out CS"); New_Line;
            -- End Critical Section -- 
            flagArray(1) := flagArray(0);
            procArray(2).Receive('U', myId, flagArray(1), null, null, false);
         end if;
      end loop;
         
      end AL_Task;
      ---- End AL_Task declaration and definition ----
      type AL_Ptr is access AL_Task;
      ptr : AL_Ptr;

   begin   
      -- Entry Point to initalize our id's from the main procedure
      accept Start (id : Integer; this : RX_Ptr; left : RX_Ptr; right : RX_Ptr; 
            thisFlag : Integer; leftFlag : Integer; rightFlag : Integer;
            numProcesses : Integer; lowId : Boolean) do
         procArray(1) := this;   
         procArray(0) := left;
         procArray(2) := right;
         flagArray(1) := thisFlag;
         flagArray(0) := leftFlag;
         flagArray(2) := rightFlag;
         num_count := numProcesses;
         myId := id;
         lowestId := lowId;
      end Start;
      ptr := new AL_Task;   
         
      -- Start our message handling loop waiting for messages to arrive -- 
      loop 
         accept Receive (mesgType : Character; id : Integer; flag : Integer; 
               left : RX_Ptr; right : RX_Ptr; lowId : Boolean) do
            case mesgType is
               when 'U' => 
                  begin
			Put(id, (80 - 10 ) );
			Put_Line(" update");
                     New_Line;   
                     -- As we always receive update messages from the left neighbour
                     -- we simply change our local copy of left's new flag
                     flagArray(0) := flag;
                  end;
               when 'N' => 
                  begin
			Put(id, (80 - 10 ) );
			Put_Line(" neighbor");
                     New_Line;   
                     if (left = null) then -- Someone is updating our right neighbour
                        procArray(2) := right;
                        flagArray(2) := flag;
                     else -- Someone is updating our left neighbour
                        procArray(0) := left;
                        flagArray(0) := flag;
                        lowestId := lowId;   
                     end if;
                  end;
               when 'R' => 
                  begin
			Put(id, (80 - 10 ) );
			Put_Line(" dying...");
                     procArray(0).Receive('N', myId, flagArray(2), 
                        null, procArray(2), false); -- Tell my left who my right was
                     -- Also, if dying, we pass true as the lowest id to the right,
                     -- as it is our next lowest, if we were the lowest already
                     if (lowestId) then
                        procArray(2).Receive('N', myId, flagArray(0), procArray(0),
                            null, true); -- Tell my right who my left was
                     else
                        procArray(2).Receive('N', myId, flagArray(0), procArray(0),
                            null, false); -- Tell my right who my left was
                     end if;   
                     ShouldIDie := true; -- Set flag to drop out of loop now
                  end;
               when Others => null;
            end case;
         end Receive;      
      exit when ShouldIDie = true;
      end loop;
   end RX_Task;
   ---- End RX_Task declaration and definition ----

   Procedure Driver is
	numtasks_user, seed_user : Integer; --user defined at runtime by std. in
      numProcesses : Integer;
      ptrArray : array (0..15) of RX_Ptr;

	killId		:	Integer;									--temp to store which process to kill
  RIPArray	: array (0..100) of Boolean := (Others => FALSE);

   begin
	put("# random seed:       ");
	get(seed_user); --to ensure a significantly random series, a seed is needed
									-- to generate pseudo-random numbers
 	Ada.Numerics.Float_Random.Reset(g,seed_user);
		--seed the random number pool

		put("# tasks[1-50]:       ");
      Get (numProcesses);

      -- Reset the random number generator
      
      -- Create each of our tasks
      for count in 0 .. numProcesses-1
      loop
         ptrArray(count) := new RX_Task;
      end loop;            
      
      -- Now pass everyone their neighbours and initialize
      -- We pass 0 separately in order to set it to lowest ID
      ptrArray(0).Start(0, ptrArray(0), ptrArray((0 - 1) mod numProcesses),
            ptrArray((0 + 1) mod numProcesses), 0, ((0 - 1) mod numProcesses),
            ((0 + 1) mod numProcesses), numProcesses, true);
      for count in 1 .. numProcesses-1
      loop
         ptrArray(count).Start(count, ptrArray(count), 
            ptrArray((count - 1) mod numProcesses), 
            ptrArray((count + 1) mod numProcesses), count, 
            ((count - 1) mod numProcesses),((count + 1) mod numProcesses),
             numProcesses, false);
      end loop;

    delay (60.0);
   
	FOR kill_index IN REVERSE 0 .. (numprocesses - 1)
	LOOP
    delay (3.0);
		Put ("Going to kill random process ... process "); 
     -- Delay 60 seconds then kill off a process -- 
		LOOP
			killID := Integer (random(g)) mod numprocesses;
			EXIT WHEN NOT RIParray(killID);
		END LOOP;
		RIParray(killID) := TRUE;
		Put(killID, 0); Put_line("");
		ptrArray(killID).Receive('R', 0, 0, null, null, false);
  END LOOP; --kill out all our processes

   end Driver;

begin
   -- Call our driver procedure (separate to keep variables from being global)
   Driver;
   
end main;
