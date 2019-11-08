%
% Name:          robaff_dlv.sp
% Author:        Ben Meadows
% Date created:  2019-07-22
% Date modified: 2019-11-07
%

% Recommended to run: SPARC version 2.48 or greater.
% Example usage in Windows command line:
% java -jar sparc.jar robaff_dlv.sp -solver dlv -A > plans_out.txt

#maxint = 1000.

#const numSteps = 6.

%% Domain: 3 rooms connected to a corridor with two internal doors, offering alternative paths from end to end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  sorts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#location = [rm][0..3].
#robot = {rob0}.
#person = {per0, per1}.
#entity = #robot + #person.
#type = {office, library, workshop, hallway}.
#fragility = {fragile, standard, robust}. %#status = {damaged, intact}.
#state = {open, closed, locked}.
#weight = {light, middle, heavy}.
#force = {weak, strong}.
#friction = {smooth, rough}.
#digits = {2, 3, 4}.
#actuation_mode = {electric, pneumatic}.
#articulation = {pinch_friction, flexible_conform}.
#floor = {tile, concrete, carpet}.
#door = [door][0..4].
#value = {very_low, low, moderate, high, very_high}.
#mapnum = [0..4].
#n = 0..999.
#package = {package0}.
#textbook = {text0, text1, text2}.
#object = #textbook + #package.
#thing = #object + #entity.
#step = 0..numSteps.

% Affordance metrics --
% energy:  Resource cost of performing the action
% risk:    Chance of the action failing or having side-effects
% time:    The time taken to perform the action
#metric = {energy, risk, time}.

#id = [id][1..35].

%% Fluents
#inertial_fluent = loc(#thing, #location) + in_hand(#entity, #object) + d_state(#door, #state).
#fluent = #inertial_fluent. % + #defined_fluent.
#action = move(#robot, #location) + pickup(#robot, #force, #object) 
	+ putdown(#robot, #force, #object) + serve(#robot, #object, #person) + open_door(#robot, #force, #door) + close_door(#robot, #force, #door).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

holds(#fluent,#step).
occurs(#action,#step).

success().
goal(#step). 
something_happened(#step).

% For affordance reasoning
cost(#action, #metric, #value, #step).
tcost(#action, #metric, #value, #step).
somewhat_important(#metric).
very_important(#metric).
aff_forbids(#id, #action, #step).
affordance_fails(#id, #action, #step).
forbidding_aff(#id, #action).
aff_permits(#id, #action, #step).

% Extending for plan selection
cumulative_cost(#step, #metric, #n).
total_cost(#n).
mapped(#value, #mapnum).
mapped_def(#value, #mapnum).
last_action_time_step(#step).
earlier_goal(#step).
user_weight(#metric, #value).
steps_cost(#metric, #n).
steps_multiply(#metric, #mapnum, #n).
steps_multiply_rec(#metric, #step, #n).
steps_sum(#metric, #mapnum, #n).
steps_sum_rec(#metric, #step, #n).
combinatorial_cost(#n).

multiplicative_cost(#metric, #step, #mapnum).
summed_cost(#metric, #step, #mapnum).


% For output
incurs_cost(#step, #metric, #value).
at(#step, #action).

% For navigation
access(#door, #location, #location).
accessible(#location, #location).

% Environment and object properties
loc_type(#location, #type).
%role_type(#person, #role).
ob_fragility(#object, #fragility).
ob_weight(#object, #weight).
loc_floor(#location, #floor).
d_weight(#door, #weight).

% Agent and object properties
tread_grip(#robot, #friction).
gripper_finger_number(#robot, #digits).
gripper_arm_actuation(#robot, #actuation_mode).
gripper_articulation(#robot, #articulation).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 rules			        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Plan generation control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Constraining plan generation with affordances
% Prevent actions being selected when they have a very high cost according to a very important metric
:- occurs(A,I), cost(A, Metric, very_high, I), very_important(Metric).
% Prevent actions being selected when they have a high cost according to a very important metric
:- occurs(A,I), cost(A, Metric, high, I), very_important(Metric).
% Prevent actions being selected when they have a very high cost according to a somewhat important metric
:- occurs(A,I), cost(A, Metric, very_high, I), somewhat_important(Metric).

incurs_cost(Time, Metric, X) :- occurs(Action, Time), cost(Action, Metric, X, Time).

% Predicate used for display; ensures plan is printed in order of the time steps at which actions occurred
at(S, A) :- occurs(A, S).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Causal laws
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Moving changes location to target room...
holds(loc(R, L), I+1) :- occurs(move(R, L), I).

%% Grasping an object causes object to be in hand...
holds(in_hand(R, O), I+1) :- occurs(pickup(R, F, O), I). 

%% Putting an object down causes it to no longer be in hand...
-holds(in_hand(R, O), I+1) :- occurs(putdown(R, F, O), I). 

%% Serving an object to a human causes the object to be in human's hand...
holds(in_hand(P, O), I+1) :- occurs(serve(R, O, P), I).

%% Serving an object causes the object to not be in robot's hand...
-holds(in_hand(R, O), I+1) :- occurs(serve(R, O, P), I).

holds(d_state(D, closed), I+1) :- occurs(close_door(R, F, D), I).
holds(d_state(D, open), I+1) :- occurs(open_door(R, F, D), I).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% State constraints
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Any thing exists in only one location...
-holds(loc(O, L2), I) :- holds(loc(O, L1), I), L1 != L2.

%% If a robot is holding an object, they have the same location...
holds(loc(O, L), I) :- holds(loc(R, L), I), holds(in_hand(R, O), I).

%% Only one entity can have an object in hand...
-holds(in_hand(E2, O), I) :- holds(in_hand(E1, O), I), E1 != E2.

%% Only one object can be held at any time...
-holds(in_hand(E, O2), I) :- holds(in_hand(E, O1), I), O1 != O2.

%% If thing is not at a location initially, assume not there ...
-holds(loc(Th, L), 0) :- not holds(loc(Th, L),0), #thing(Th). 

%% Location type, role type, status and weight have unique values...
-loc_type(L, T2) :- loc_type(L, T1), T1 != T2. 
%-role_type(P, RT2) :- role_type(P, RT1), RT1 != RT2.
-ob_fragility(O, S2) :- ob_fragility(O, S1), S1 != S2.
-ob_weight(O, W2) :- ob_weight(O, W1), W1 != W2.
%
-d_weight(D, W2) :- d_weight(D, W1), W1 != W2.
-tread_grip(R, F2) :- tread_grip(R, F1), F1 != F2.
-gripper_finger_number(R, N2) :- gripper_finger_number(R, N1), N1 != N2.
-gripper_arm_actuation(R, A2) :- gripper_arm_actuation(R, A1), A1 != A2.
-gripper_articulation(R, A2) :- gripper_articulation(R, A1), A1 != A2.
-loc_floor(L, F2) :- loc_floor(L, F1), F1 != F2.
-holds(d_state(D, S1), I) :- holds(d_state(D, S2), I), S1 != S2.

access(D, R2, R1) :- access(D, R1, R2).
accessible(R1, R2) :- access(D, R1, R2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Executability conditions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The affordance IDs are unique to each executability condition (more or less - might have some overlap for similar actions).
% The executability condition should have a grounded ID, although that design point should be reconsidered when implementing learning.

%% Cannot move to a location if you are already there...
-occurs(move(R, L), I) :- holds(loc(R, L), I),
							not aff_permits(id1, move(R, L), I).
%% Moving between locations possible only if there is an open door D such that access(D, L1, L2) and d_state(D, open)
-occurs(move(R, L2), I) :- holds(loc(R, L1), I), not accessible(L1, L2),
							not aff_permits(id2, move(R, L2), I).
-occurs(move(R, L2), I) :- holds(loc(R, L1), I), access(D, L1, L2), not holds(d_state(D, open), I),
							not aff_permits(id3, move(R, L2), I).

% Can only interact with an adjacent door
-occurs(open_door(R, F, D), I) :- access(D, L1, L2), -holds(loc(R, L1), I), -holds(loc(R, L2), I),
							not aff_permits(id4, open_door(R, F, D), I).
-occurs(close_door(R, F, D), I) :- access(D, L1, L2), -holds(loc(R, L1), I), -holds(loc(R, L2), I),
							not aff_permits(id5, open_door(R, F, D), I).

%% Cannot pick up an object if you are not in the same room...
-occurs(pickup(R, F, O), I) :- holds(loc(R, L1), I), holds(loc(O, L2), I), L1 != L2,
							not aff_permits(id6, pickup(R, F, O), I).

%% Cannot pick up an object already in hand... 
-occurs(pickup(R, F, O), I) :- holds(in_hand(R, O), I),
							not aff_permits(id7, pickup(R, F, O), I).

%% Rules to prevent incorrect grasping...
-occurs(pickup(R, F, O), I) :- holds(loc(R, L), I), -holds(loc(O, L), I),
							not aff_permits(id8, pickup(R, F, O), I).
-occurs(pickup(R, F, O), I) :- holds(loc(O, L), I), -holds(loc(R, L), I),
							not aff_permits(id9, pickup(R, F, O), I).

%% Cannot put down an object unless it is in hand...
-occurs(putdown(R, F, O), I) :-  not holds(in_hand(R, O), I),
							not aff_permits(id10, putdown(R, F, O), I).

%% Cannot serve an object that is not in hand...
-occurs(serve(R, O, P), I) :- not holds(in_hand(R, O), I),
							not aff_permits(id11, serve(R, O, P), I).

% Can only open a closed door (not open or locked)
-occurs(open_door(R, F, D), I) :- not holds(d_state(D, closed), I),
							not aff_permits(id12, open_door(R, F, D), I).
% Can only close an open door (not closed or locked)
-occurs(close_door(R, F, D), I) :- not holds(d_state(D, open), I),
							not aff_permits(id13, close_door(R, F, D), I).

%% Cannot serve an object unless robot and human are in same location...
-occurs(serve(R, O, P), I) :- holds(loc(R, L1), I), holds(loc(P, L2), I), L1 != L2,
							not aff_permits(id14, serve(R, O, P), I).

%% Rules to prevent incorrect serving...
-occurs(serve(R, O, P), I) :- holds(loc(R, L), I), -holds(loc(P, L), I),
							not aff_permits(id15, serve(R, O, P), I).
-occurs(serve(R, O, P), I) :- holds(loc(P, L), I), -holds(loc(R, L), I),
							not aff_permits(id16, serve(R, O, P), I).

% New executability conditions for extended robot characteristics:

% Cannot open a door with gripper_articulation=pinch_friction
-occurs(open_door(R, F, D), I) :- gripper_articulation(R, pinch_friction),
							not aff_permits(id17, open_door(R, F, D), I).

% Cannot pick up fragile object with gripper_articulation=pinch_friction
-occurs(pickup(R, F, O), I) :- ob_fragility(O, fragile), gripper_articulation(R, pinch_friction),
							not aff_permits(id18, pickup(R, F, O), I).

% Cannot pick up a moderately fragile object with gripper_arm_actuation=pneumatic
% [note: the fragile case is covered under negative affordances, below]
-occurs(pickup(R, F, O), I) :- ob_fragility(O, standard), gripper_arm_actuation(R, pneumatic),
							not aff_permits(id19, pickup(R, F, O), I).

% Cannot pick up a middle-weight object
-occurs(pickup(R, F, O), I) :- ob_weight(O, middle),
							not aff_permits(id20, pickup(R, F, O), I).

% Cannot pick up a heavy-weight object
-occurs(pickup(R, F, O), I) :- ob_weight(O, heavy),
							not aff_permits(id21, pickup(R, F, O), I).

% Cannot serve a fragile object with gripper_articulation=pinch_friction
-occurs(serve(R, O, P), I) :- ob_fragility(O, fragile), gripper_articulation(R, pinch_friction),
							not aff_permits(id22, serve(R, O, P), I).

% Cannot serve a human with gripper_arm_actuation=pneumatic
-occurs(serve(R, O, P), I) :- gripper_arm_actuation(R, pneumatic),
							not aff_permits(id23, serve(R, O, P), I).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Positive affordances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% A positive affordance for debugging purposes: Robot can always serve objects directly to an engineer's hands
%aff_permits(id12, serve(R, O, P), I) :- role_type(P, engineer).
%aff_permits(id13, serve(R, O, P), I) :- role_type(P, engineer).
%aff_permits(id14, serve(R, O, P), I) :- role_type(P, engineer).

% Cannot open a door with gripper_articulation=pinch_friction UNLESS robot has 3 or 4 fingers
aff_permits(id17, open_door(R, F, D), I) :- gripper_finger_number(R, N), N > 2.

% Cannot pick up fragile object with gripper_articulation=pinch_friction UNLESS robot has 4 fingers
aff_permits(id18, pickup(R, F, O), I) :- gripper_finger_number(R, 4).

% Cannot pick up a moderately fragile object with gripper_arm_actuation=pneumatic UNLESS gripper_articulation=flexible_conform
aff_permits(id19, pickup(R, F, O), I) :- gripper_articulation(R, flexible_conform).

% Cannot pick up a middle-weight object UNLESS EITHER using a strong force, or with gripper_arm_actuation=pneumatic, or the robot has 3 or 4 fingers
aff_permits(id20, pickup(R, strong, O), I).
aff_permits(id20, pickup(R, F, O), I) :- gripper_arm_actuation(R, pneumatic).
aff_permits(id20, pickup(R, F, O), I) :- gripper_finger_number(R, N), N > 2.

% Cannot pick up a heavy-weight object UNLESS gripper_arm_actuation=pneumatic AND the robot has 3 or 4 fingers
aff_permits(id21, pickup(R, F, O), I) :- gripper_arm_actuation(R, pneumatic), gripper_finger_number(R, N), N > 2.

% Cannot serve a fragile object with gripper_articulation=pinch_friction UNLESS robot has 4 fingers
aff_permits(id22, serve(R, O, P), I) :- gripper_finger_number(R, 4).

% Cannot serve a human with gripper_arm_actuation=pneumatic UNLESS gripper_articulation=flexible_conform
aff_permits(id23, serve(R, O, P), I) :- gripper_articulation(R, flexible_conform).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Negative affordances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Can't pick up a fragile object with a pneumatic arm actuation
forbidding_aff(id31, pickup(R, F, O)).
affordance_fails(id31, pickup(R, F, O), I) :- not ob_fragility(O, fragile). % In this circumstance the forbidding affordance fails (doesn't apply)
affordance_fails(id31, pickup(R, F, O), I) :- not gripper_arm_actuation(R, pneumatic). % In this circumstance the forbidding affordance fails (doesn't apply)

% Can't pick up a heavy object with 'flexible_conform' gripper articulation
forbidding_aff(id32, pickup(R, F, O)).
affordance_fails(id32, pickup(R, F, O), I) :- not ob_weight(O, heavy). % In this circumstance the forbidding affordance fails (doesn't apply)
affordance_fails(id32, pickup(R, F, O), I) :- not gripper_articulation(R, flexible_conform). % In this circumstance the forbidding affordance fails (doesn't apply)

% Can't pick up a heavy object with weak force
forbidding_aff(id33, pickup(R, F, O)).
affordance_fails(id33, pickup(R, F, O), I) :- not ob_weight(O, heavy). % In this circumstance the forbidding affordance fails (doesn't apply)
affordance_fails(id33, pickup(R, strong, O), I). % In this circumstance the forbidding affordance fails (doesn't apply)

% Axioms that were rules to be learned in previous work
% Now largely subsumed into particularly costly affordance gradations
%-occurs(serve(R, O, P), I) :- role_type(P, engineer), 
			      %loc_type(L, workshop), 
			      %holds(loc(P, L), I).
%-occurs(serve(R, O, P), I) :- ob_status(O, damaged).
%-occurs(pickup(R, F, O), I) :- ob_weight(O, heavy).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Inertial axiom
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% General inertia axioms...
holds(F,I+1) :- #inertial_fluent(F),
                holds(F,I),
                not -holds(F,I+1).

-holds(F,I+1) :- #inertial_fluent(F),
                 -holds(F,I),
                 not holds(F,I+1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Closed world assumption
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% CWA for Actions...
-occurs(A,I) :- not occurs(A,I).

%% CWA for defined fluents...
%-holds(F, I) :- #defined_fluent(F), not holds(F, I). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Planning module
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Permitting and forbidding affordance relations
aff_forbids(ID, Action, I) :- forbidding_aff(ID, Action), not affordance_fails(ID, Action, I).
-occurs(Action, I) :- aff_forbids(ID, Action, I).

%% Failure is not an option
success :- goal(I).
:- not success. 

%% Cannot be idle while goal remains unachieved
occurs(A, I) | -occurs(A, I) :- not goal(I).

%% Cannot act after goal met
:- goal(I1), occurs(A, I2), I2 > I1.

%% test
:- goal(I), occurs(A, I).

%% Cannot execute two actions at the same time
:- occurs(A1,I), occurs(A2,I), A1 != A2.

something_happened(I) :- occurs(A, I).

:- not goal(I),
   not something_happened(I).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% => begin extension for plan selection

:~ total_cost(N). [N:1]			% Minimise the total cost of the plan





mapped(X, Y) :- mapped_def(X, Y).
mapped(X, 0) :- not mapped_def(X, 1), not mapped_def(X, 2), not mapped_def(X, 3), not mapped_def(X, 4).
mapped_def(very_low, 0).
mapped_def(low, 1).
mapped_def(moderate, 2).
mapped_def(high, 3).
mapped_def(very_high, 4).

last_action_time_step(I) :- not goal(0), goal(I+1), not earlier_goal(I+1). % Time step of last action can be 0...numSteps.
earlier_goal(I) :- goal(J), J < I.

% The user gives just three preferences. These are used in both plan generation, to preclude plans incorporating actions whose costs exceed certain bounds, and in plan selection, to weight possibilities.
user_weight(risk, very_high).
user_weight(energy, high).
user_weight(time, moderate).


% DISABLED BY DEBUG
steps_cost(Metric, 0) :-		user_weight(Metric, Val), mapped(Val, 0).
steps_cost(risk, Total) :-		user_weight(risk, Val), mapped(Val, N), N > 0, steps_multiply(risk, N, Total). % product divided by plan length
%%%%%%steps_cost(risk, Total) :-		user_weight(risk, Val), mapped(Val, N), N > 0, steps_sum(risk, N, Total). % DEBUG
steps_cost(energy, Total) :-	user_weight(energy, Val), mapped(Val, N), N > 0, steps_sum(energy, N, Total). % sum
steps_cost(time, Total) :-		user_weight(time, Val), mapped(Val, N), N > 0, steps_sum(time, N, Total). % sum

% DEBUG
%steps_cost(Metric, T) :- incurs_cost(0, Metric, Value0), mapped(Value0, T0), incurs_cost(1, Metric, Value1), mapped(Value1, T1), incurs_cost(2, Metric, Value2), mapped(Value2, T2), T = T0+T1+T2.


% It should be possible to use different combinatorial functions for the total plan cost, based on the costs according to different weighted metrics.
% There are three obvious options.

% Option 1: Overall plan cost is the sum of costs according to the different weighted metrics
combinatorial_cost(Number) :-
	steps_cost(energy, V1),
	steps_cost(risk, V2),
	steps_cost(time, V3),
	Number = V1+V2+V3.

% Option 2: Overall plan cost is equal to the single worst cost according to one weighted metric
%worst_cost(Number) :- ...

% Option 3: Overall plan cost is equal to the plan cost according to the metric with the highest user weight
%most_important_cost(Number) :- ...

total_cost(Number) :- combinatorial_cost(Number).
%total_cost(Number) :- worst_cost(Number).
%total_cost(Number) :- most_important_cost(Number).



multiplicative_cost(Metric, T, 1) :-
	incurs_cost(T, Metric, Value),
	mapped(Value, 0).
multiplicative_cost(Metric, T, Number) :-
	incurs_cost(T, Metric, Value),
	mapped(Value, Number),
	Number != 0.
	
summed_cost(Metric, T, Number) :-
	incurs_cost(T, Metric, Value),
	mapped(Value, Number).
	
steps_sum(Metric, Weight, Total) :-
	last_action_time_step(Step),
	steps_sum_rec(Metric, Step, T),
	Total = Weight * T.

steps_sum_rec(Metric, 0, Total) :-
	summed_cost(Metric, 0, Total).
steps_sum_rec(Metric, Step, Total) :-
	Step > 0,
	steps_sum_rec(Metric, Step-1, Previous),
	summed_cost(Metric, Step, Cost),
	Total = Previous + Cost.
	
steps_multiply(Metric, Weight, Total) :-
	last_action_time_step(Step),
	steps_multiply_rec(Metric, Step, T),
	Total = (Weight * T) / (Step + 1).

steps_multiply_rec(Metric, 0, Total) :-
	multiplicative_cost(Metric, 0, Total).
steps_multiply_rec(Metric, Step, Total) :-
	Step > 0,
	steps_multiply_rec(Metric, Step-1, Previous),
	multiplicative_cost(Metric, Step, Cost),
	Total = Previous * Cost.
	
	
% steps_multiply(Metric, N, Total) :-
	% last_action_time_step(0),
	% multiplicative_cost(Metric, 0, V0),
	% Total = N * V0.
% steps_multiply(Metric, N, Total) :-
	% last_action_time_step(1),
	% multiplicative_cost(Metric, 0, V0),
	% multiplicative_cost(Metric, 1, V1),
	% Total = N * (V0 * V1)/2.
% steps_multiply(Metric, N, Total) :-
	% last_action_time_step(2),
	% multiplicative_cost(Metric, 0, V0),
	% multiplicative_cost(Metric, 1, V1),
	% multiplicative_cost(Metric, 2, V2),
	% Total = N * (V0 * V1 * V2)/3.
% steps_multiply(Metric, N, Total) :-
	% last_action_time_step(3),
	% multiplicative_cost(Metric, 0, V0),
	% multiplicative_cost(Metric, 1, V1),
	% multiplicative_cost(Metric, 2, V2),
	% multiplicative_cost(Metric, 3, V3),
	% Total = N * (V0 * V1 * V2 * V3)/4.
% steps_multiply(Metric, N, Total) :-
	% last_action_time_step(4),
	% multiplicative_cost(Metric, 0, V0),
	% multiplicative_cost(Metric, 1, V1),
	% multiplicative_cost(Metric, 2, V2),
	% multiplicative_cost(Metric, 3, V3),
	% multiplicative_cost(Metric, 4, V4),
	% Total = N * (V0 * V1 * V2 * V3 * V4)/5.
% steps_multiply(Metric, N, Total) :-
	% last_action_time_step(5),
	% multiplicative_cost(Metric, 0, V0),
	% multiplicative_cost(Metric, 1, V1),
	% multiplicative_cost(Metric, 2, V2),
	% multiplicative_cost(Metric, 3, V3),
	% multiplicative_cost(Metric, 4, V4),
	% multiplicative_cost(Metric, 5, V5),
	% Total = N *(V0 * V1 * V2 * V3 * V4 * V5)/6.
% steps_multiply(Metric, N, Total) :-
	% last_action_time_step(6),
	% multiplicative_cost(Metric, 0, V0),
	% multiplicative_cost(Metric, 1, V1),
	% multiplicative_cost(Metric, 2, V2),
	% multiplicative_cost(Metric, 3, V3),
	% multiplicative_cost(Metric, 4, V4),
	% multiplicative_cost(Metric, 5, V5),
	% multiplicative_cost(Metric, 6, V5),
	% Total = N * (V0 * V1 * V2 * V3 * V4 * V5 * V6)/7.

% steps_sum(Metric, N, Total) :-
	% last_action_time_step(0),
	% summed_cost(Metric, 0, V0),
	% Total = N * V0.
% steps_sum(Metric, N, Total) :-
	% last_action_time_step(1),
	% summed_cost(Metric, 0, V0),
	% summed_cost(Metric, 1, V1),
	% Total = N * (V0 + V1).
% steps_sum(Metric, N, Total) :-
	% last_action_time_step(2),
	% summed_cost(Metric, 0, V0),
	% summed_cost(Metric, 1, V1),
	% summed_cost(Metric, 2, V2),
	% Total = N * (V0 + V1 + V2).
% steps_sum(Metric, N, Total) :-
	% last_action_time_step(3),
	% summed_cost(Metric, 0, V0),
	% summed_cost(Metric, 1, V1),
	% summed_cost(Metric, 2, V2),
	% summed_cost(Metric, 3, V3),
	% Total = N * (V0 + V1 + V2 + V3).
% steps_sum(Metric, N, Total) :-
	% last_action_time_step(4),
	% summed_cost(Metric, 0, V0),
	% summed_cost(Metric, 1, V1),
	% summed_cost(Metric, 2, V2),
	% summed_cost(Metric, 3, V3),
	% summed_cost(Metric, 4, V4),
	% Total = N * (V0 + V1 + V2 + V3 + V4).
% steps_sum(Metric, N, Total) :-
	% last_action_time_step(5),
	% summed_cost(Metric, 0, V0),
	% summed_cost(Metric, 1, V1),
	% summed_cost(Metric, 2, V2),
	% summed_cost(Metric, 3, V3),
	% summed_cost(Metric, 4, V4),
	% summed_cost(Metric, 5, V5),
	% Total = N * (V0 + V1 + V2 + V3 + V4 + V5).
% steps_sum(Metric, N, Total) :-
	% last_action_time_step(6),
	% summed_cost(Metric, 0, V0),
	% summed_cost(Metric, 1, V1),
	% summed_cost(Metric, 2, V2),
	% summed_cost(Metric, 3, V3),
	% summed_cost(Metric, 4, V4),
	% summed_cost(Metric, 5, V5),
	% summed_cost(Metric, 6, V5),
	% Total = N * (V0 + V1 + V2 + V3 + V4 + V5 + V6).



% => end extension for plan selection
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%








%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test scenario: State setup
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Fluents
holds(loc(rob0, rm0), 0).
holds(loc(text0, rm0), 0). 
holds(loc(text1, rm1), 0). 
holds(loc(text2, rm2), 0). 
holds(loc(package0, rm1), 0). 
holds(loc(per0, rm2), 0).
holds(loc(per1, rm3), 0).
holds(d_state(door0, closed), 0).
holds(d_state(door1, closed), 0).
holds(d_state(door2, closed), 0).
holds(d_state(door3, closed), 0).
holds(d_state(door4, closed), 0).

% Statics (objects)
ob_weight(text0, light).
ob_fragility(text0, standard).
ob_weight(text1, heavy).
ob_fragility(text1, robust).
ob_weight(text2, light).
ob_fragility(text2, robust).
ob_weight(package0, middle).
ob_fragility(package0, fragile).

% Statics (locations)
loc_type(rm0, hallway).
loc_type(rm1, library).
loc_type(rm2, workshop).
loc_type(rm3, office).
loc_floor(rm0, tile).
loc_floor(rm1, carpet).
loc_floor(rm2, concrete).
loc_floor(rm3, carpet).
access(door0, rm0, rm1).
access(door1, rm0, rm2).
access(door2, rm0, rm3).
access(door3, rm1, rm2).
access(door4, rm2, rm3).
d_weight(door0, light). % Note the robot's grasp uses pinch friction in this scenario, increasing risk with heavier doors -- and risk is less tolerated
d_weight(door1, light).
d_weight(door2, light).
d_weight(door3, middle).
d_weight(door4, heavy).

%    +-------------------------------------+
%    |                 rm0                 |
%    +-------------------------------------+
%     |     	        |                 |
% -[door0]-         -[door1]-         -[door2]-
%     |     		    |                 |
%    ---               ---               ---
%    rm1   -[door3]-   rm2   -[door4]-   rm3
%    ---               ---               ---

% Statics (agent properties)
% (Comment these out in order for test_runner.pl to automatically insert parameters after the symbol $ below)
tread_grip(rob0, smooth). % 2 possibilities
gripper_finger_number(rob0, 3). % 3 possibilities
gripper_arm_actuation(rob0, electric). % 2 possibilities
gripper_articulation(rob0, pinch_friction). % 2 possibilities
% $


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test scenario: Meta
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Different levels of 'importance' alter which plans get discarded upon generation
% By default, a metric that is not specified will be considered unimportant
very_important(Metric) :- user_weight(Metric, X), mapped(X, Y), Y > 3.
somewhat_important(Metric) :- user_weight(Metric, X), mapped(X, Y), Y > 1.

%% Goal:
%goal(I) :- holds(loc(text0, rm1), I), -holds(in_hand(rob0, text0), I).
%goal(I) :- holds(in_hand(per0, text0), I).

%goal(I) :- holds(in_hand(per0, T), I), #textbook(T).
goal(I) :- holds(in_hand(P, T), I), #person(P), #textbook(T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test scenario: Affordance costs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% These rules introduced for testing purposes; e.g., when forbidding affordances are elided, the corresponding actions should be considered possible and have a default cost.
cost(A, Metric, Val, I) :- tcost(A, Metric, Val, I).
cost(A, Metric, moderate, I) :- not tcost(A, Metric, very_low, I), not tcost(A, Metric, low, I), not tcost(A, Metric, high, I), not tcost(A, Metric, very_high, I). % default

% Moving is a mid-cost, low-risk action
% It takes more time on carpeted floor surfaces, and on cluttered floor surfaces (library, office)
% The amount of friction offered by the robot's tread increases the energy requirement on carpeted surfaces (chance of sticking) but lowers the risk of failure on tiled surfaces (better grip)
tcost(move(R, L), time, high, I) :- holds(loc(R, LX), I), loc_type(LX, office), loc_floor(LX, carpet).
tcost(move(R, L), time, high, I) :- holds(loc(R, LX), I), loc_type(LX, library), loc_floor(LX, carpet).
tcost(move(R, L), time, moderate, I) :- holds(loc(R, LX), I), loc_floor(LX, carpet), loc_type(LX, T), T != office, T != library.
tcost(move(R, L), time, moderate, I) :- holds(loc(R, LX), I), loc_type(LX, office), loc_floor(LX, F), F != carpet.
tcost(move(R, L), time, moderate, I) :- holds(loc(R, LX), I), loc_type(LX, library), loc_floor(LX, F), F != carpet.
tcost(move(R, L), time, low, I) :- holds(loc(R, LX), I), loc_type(LX, workshop), loc_floor(LX, F), F != carpet.
tcost(move(R, L), time, low, I) :- holds(loc(R, LX), I), loc_type(LX, hallway), loc_floor(LX, F), F != carpet.
%
tcost(move(R, L), energy, moderate, I) :- holds(loc(R, LX), I), loc_floor(LX, carpet), tread_grip(R, smooth).
tcost(move(R, L), energy, high, I) :- holds(loc(R, LX), I), loc_floor(LX, carpet), tread_grip(R, rough).
tcost(move(R, L), energy, low, I) :- holds(loc(R, LX), I), loc_floor(LX, F), F != carpet.
%
tcost(move(R, L), risk, moderate, I) :- holds(loc(R, LX), I), loc_floor(LX, tile), tread_grip(R, smooth).
tcost(move(R, L), risk, low, I) :- holds(loc(R, LX), I), loc_floor(LX, tile), tread_grip(R, rough).
tcost(move(R, L), risk, very_low, I) :- holds(loc(R, LX), I), loc_floor(LX, F), F != tile.

% Picking an object up: the heavier the object and weaker the force used, the higher the risk of failure.
tcost(pickup(R, strong, O), risk, very_high, I) :- ob_weight(O, heavy), gripper_arm_actuation(R, pneumatic), gripper_finger_number(R, 3). % Positive affordance negating normal impossibility - still very_high risk
tcost(pickup(R, strong, O), risk, high, I)      :- ob_weight(O, heavy), gripper_arm_actuation(R, pneumatic), gripper_finger_number(R, 4). % Positive affordance negating normal impossibility - still high risk
%
tcost(pickup(R, strong, O), risk, moderate, I) :- ob_weight(O, middle). % Positive affordance negating normal impossibility - still moderate risk
tcost(pickup(R, F, O), risk, moderate, I)      :- ob_weight(O, middle), gripper_arm_actuation(R, pneumatic). % Positive affordance negating normal impossibility - still moderate risk
tcost(pickup(R, F, O), risk, moderate, I)      :- ob_weight(O, middle), gripper_finger_number(R, N), N > 2. % Positive affordance negating normal impossibility - still moderate risk
%
% For light objects, risk is low UNLESS at least one of the following conditions hold (in which case risk is very low):
%   {strong force, >2 fingers, electric actuation, flexible_conform articulation}
tcost(pickup(R, strong, O), risk, very_low, I) :- ob_weight(O, light).
tcost(pickup(R, F, O), risk, very_low, I)      :- ob_weight(O, light), gripper_finger_number(R, N), N > 2.
tcost(pickup(R, F, O), risk, very_low, I)      :- ob_weight(O, light), gripper_arm_actuation(R, electric).
tcost(pickup(R, F, O), risk, very_low, I)      :- ob_weight(O, light), gripper_articulation(R, flexible_conform).
tcost(pickup(R, weak, O), risk, low, I)     :- ob_weight(O, light), gripper_finger_number(R, N), N < 3, -gripper_arm_actuation(R, electric), -gripper_articulation(R, flexible_conform).
%%%%% Note that there are forbidding/permitting affordances relating to both weight and fragility for 'pickup'.
%%%%% For simplicity, as long as factors relating to the object's fragility allow it to be picked up, only factors relating to its weight then have bearing on the affordance costs.
%
% Robust objects (specifically) can be picked up with less care, and therefore faster.
tcost(pickup(R, F, O), time, very_low, I) :- ob_fragility(O, robust), gripper_arm_actuation(R, electric).
tcost(pickup(R, F, O), time, low, I) :- ob_fragility(O, robust), gripper_arm_actuation(R, pneumatic).
tcost(pickup(R, F, O), time, moderate, I) :- ob_fragility(O, Frag), Frag != robust.
% The greater the force used, the more energy required to pick an object up.
tcost(pickup(R, weak, O), energy, moderate, I) :- gripper_arm_actuation(R, electric).
tcost(pickup(R, weak, O), energy, low, I) :- gripper_arm_actuation(R, pneumatic).
tcost(pickup(R, strong, O), energy, high, I) :- gripper_arm_actuation(R, electric).
tcost(pickup(R, strong, O), energy, moderate, I) :- gripper_arm_actuation(R, pneumatic).

% Putting an object down: using a greater force has more risk and more energy but less time
tcost(putdown(R, weak, O), energy, very_low, I).
tcost(putdown(R, weak, O), risk, low, I).
tcost(putdown(R, weak, O), time, low, I).
tcost(putdown(R, strong, O), energy, low, I).
tcost(putdown(R, strong, O), risk, moderate, I).
tcost(putdown(R, strong, O), time, very_low, I).

% Opening a door is a high-time action. The heavier the door and the weaker the force used, the higher the risk of failure; the greater the force used, the more energy required
tcost(open_door(R, F, D), time, high, I).
%
tcost(open_door(R, weak, D), energy, low, I).
tcost(open_door(R, strong, D), energy, high, I).
%
tcost(open_door(R, F, D), risk, moderate, I) :-      d_weight(D, light), gripper_articulation(R, pinch_friction), gripper_finger_number(R, N), N > 2. % Positive affordance negating normal impossibility - still moderate risk
tcost(open_door(R, weak, D), risk, high, I) :-       d_weight(D, middle), gripper_articulation(R, pinch_friction), gripper_finger_number(R, N), N > 2. % Positive affordance negating normal impossibility - still high risk
tcost(open_door(R, strong, D), risk, moderate, I) :- d_weight(D, middle), gripper_articulation(R, pinch_friction), gripper_finger_number(R, N), N > 2. % Positive affordance negating normal impossibility - still moderate risk
tcost(open_door(R, weak, D), risk, very_high, I) :-  d_weight(D, heavy), gripper_articulation(R, pinch_friction), gripper_finger_number(R, N), N > 2. % Positive affordance negating normal impossibility - still very high risk
tcost(open_door(R, strong, D), risk, moderate, I) :- d_weight(D, heavy), gripper_articulation(R, pinch_friction), gripper_finger_number(R, N), N > 2. % Positive affordance negating normal impossibility - still moderate risk
%
tcost(open_door(R, weak, D), risk, low, I) :- d_weight(D, light), -gripper_articulation(R, pinch_friction).
% tcost(open_door(R, weak, D), risk, low, I) :- d_weight(D, light), gripper_finger_number(R, N), N < 3. % Not strictly necessary, given previous line, due to the original forbidding affordance
tcost(open_door(R, weak, D), risk, moderate, I) :- d_weight(D, middle), -gripper_articulation(R, pinch_friction).
% tcost(open_door(R, weak, D), risk, moderate, I) :- d_weight(D, middle), gripper_finger_number(R, N), N < 3. % Not strictly necessary, given previous line, due to the original forbidding affordance
tcost(open_door(R, weak, D), risk, high, I) :- d_weight(D, heavy), -gripper_articulation(R, pinch_friction).
% tcost(open_door(R, weak, D), risk, high, I) :- d_weight(D, heavy), gripper_finger_number(R, N), N < 3. % Not strictly necessary, given previous line, due to the original forbidding affordance
tcost(open_door(R, strong, D), risk, very_low, I) :- d_weight(D, light), -gripper_articulation(R, pinch_friction).
% tcost(open_door(R, strong, D), risk, very_low, I) :- d_weight(D, light), gripper_finger_number(R, N), N < 3. % Not strictly necessary, given previous line, due to the original forbidding affordance
tcost(open_door(R, strong, D), risk, very_low, I) :- d_weight(D, middle), -gripper_articulation(R, pinch_friction).
% tcost(open_door(R, strong, D), risk, very_low, I) :- d_weight(D, middle), gripper_finger_number(R, N), N < 3. % Not strictly necessary, given previous line, due to the original forbidding affordance
tcost(open_door(R, strong, D), risk, low, I) :- d_weight(D, heavy), -gripper_articulation(R, pinch_friction).
% tcost(open_door(R, strong, D), risk, low, I) :- d_weight(D, heavy), gripper_finger_number(R, N), N < 3. % Not strictly necessary, given previous line, due to the original forbidding affordance

% Closing a door is similar to opening it, but faster and with less risk of failure
tcost(close_door(R, F, D), time, low, I).
tcost(close_door(R, weak, D), energy, low, I).
tcost(close_door(R, strong, D), energy, high, I).
tcost(close_door(R, weak, D), risk, very_low, I) :- d_weight(D, light).
tcost(close_door(R, strong, D), risk, very_low, I) :- d_weight(D, light).
tcost(close_door(R, weak, D), risk, low, I) :- d_weight(D, middle).
tcost(close_door(R, strong, D), risk, very_low, I) :- d_weight(D, middle).
tcost(close_door(R, weak, D), risk, moderate, I) :- d_weight(D, heavy).
tcost(close_door(R, strong, D), risk, very_low, I) :- d_weight(D, heavy).

% Serving an object has middling costs, with a greater risk for serving a more fragile object
tcost(serve(R, O, P), time, high, I)      :- ob_fragility(O, fragile), gripper_articulation(R, pinch_friction), gripper_finger_number(R, 4). % Positive affordance negating normal impossibility - still high time
tcost(serve(R, O, P), time, moderate, I)  :- -ob_fragility(O, fragile).
tcost(serve(R, O, P), time, moderate, I)  :- -gripper_articulation(R, pinch_friction).
% tcost(serve(R, O, P), time, moderate, I)  :- gripper_finger_number(R, N), N < 4. % Not strictly necessary, given the previous two lines, due to the original forbidding affordance

%
tcost(serve(R, O, P), risk, high, I)      :- ob_fragility(O, fragile), -gripper_arm_actuation(R, pneumatic), -gripper_articulation(R, pinch_friction).
tcost(serve(R, O, P), risk, very_high, I) :- -gripper_arm_actuation(R, pneumatic), ob_fragility(O, fragile), gripper_articulation(R, pinch_friction), gripper_finger_number(R, 4). % Positive affordance negating normal impossibility - still very_high risk
tcost(serve(R, O, P), risk, very_high, I) :- gripper_arm_actuation(R, pneumatic), gripper_articulation(R, flexible_conform). % Positive affordance negating normal impossibility - still very high risk
tcost(serve(R, O, P), risk, low, I) 		 :- -gripper_arm_actuation(R, pneumatic), ob_fragility(O, robust).
tcost(serve(R, O, P), risk, moderate, I)  :- -gripper_arm_actuation(R, pneumatic), ob_fragility(O, standard).
%
tcost(serve(R, O, P), energy, moderate, I).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
display
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Uncomment to include typical ASP output
%occurs.
%holds.
%%%%-holds.

% Display the plan, ordered, and the associated costs
at.
incurs_cost.
combinatorial_cost.
total_cost.
goal.
last_action_time_step.
steps_cost.

%permit_total.
%plan_selected.
%cumulative_cost.
%multi_cost.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Notes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ===================================================================
% Robot physical characteristics are meant to influence the agent's capabilities when it comes to exerting force with the gripper and arm.
% We can to some extent think of them as influencing more general capabilities not overtly given in the domain, such as precision, power and safety.
% e.g., more gripper fingers increases precision, flexible gripper articulation increases safety, and a pneumatic arm increases power but decreases safety.
% These relate to
% (a) the capability to carry out tasks (e.g., human should not be served unless safe, heavy object cannot be lifted without power, door cannot be opened without precision), and
% (b) the gradations along which actions in particular contexts (including the context of agent chararacteristics) differ by costs along different metrics.
% ===================================================================
% Specification:
% can't open door with gripper_articulation=pinch_friction UNLESS (pos_aff) gripper_finger_number=3 or 4
% can't pick up fragile object with gripper_articulation=pinch_friction UNLESS (pos_aff) gripper_finger_number=4
% can't serve fragile object with gripper_articulation=pinch_friction UNLESS (pos_aff) gripper_finger_number=4
% can't serve human with gripper_arm_actuation=pneumatic UNLESS (pos_aff) gripper_articulation=flexible_conform
% can't pick up fragile object with gripper_arm_actuation=pneumatic
% can't pick up a standard fragility object with gripper_arm_actuation=pneumatic UNLESS (pos_aff) gripper_articulation=flexible_conform
% can't pick up middle-weight object UNLESS EITHER strong force, gripper_arm_actuation=pneumatic, gripper_finger_number=3, or gripper_finger_number=4
% can't pick up heavy object using strong force UNLESS gripper_arm_actuation=pneumatic AND EITHER gripper_finger_number=3 or gripper_finger_number=4
% can't pick up heavy object using strong force with gripper_articulation=flexible_conform
% can't pick up heavy object with weak force
% ===================================================================
% Other future possibilities for robot gripper characteristics:
% 1. Motion type -- angular (fingers rotate on axis) or parallel (fingers approach each other parallel)
% 2. Size of gripper
% ===================================================================

