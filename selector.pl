/*
 * Name:          selector.pl
 * Author:        Ben Meadows
 * Date created:  2019-02-05
 * Date modified: 2019-02-21
 */

:- dynamic plan_cost_line/1, newID/1, plan_steps/3, plan_cost/4, overall_cost/2, selected/1, input_file/1, output_file/1.

newID(1).
cost_metrics([risk, energy, time]).
cost_degrees([very_low, low, moderate, high, very_high]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% 1. Main %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Recommended to run: SWI-Prolog version 8.0.1 or greater.
% Example usage in Prolog shell:
% run('plans_sample.txt', 'out.txt').

% This function, after plans and their various costs have been generated in an answer set format, collates the data, and selects a plan based on a provided function.
% It outputs selection criteria, the selected plan, and the plan's score according to those criteria.
run(In, Out) :-
	read_input(In),
	parse_input,
	select_plan,
	print_results(Out),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% 2. Read in answer sets %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_input(F) :-
	retractall(input_file(_)),
	asserta(input_file(F)),
	open(F, read, Str),
	read_all_sets(Str).

read_all_sets(File) :-
    read_line_to_codes(File, Codes),
	Codes \= end_of_file,
	length(Codes,X),
	!,
	(X == 0 -> true ;
	(
		atom_string(String, Codes),
		sub_string(String, 1, _Length, 1, Content),
		string_concat('[', Content, String2),
		string_concat(String2, ']', String3),
		term_string(List, String3),
		asserta(plan_cost_line(List))
	)),
	read_all_sets(File).
read_all_sets(_) :- !.

replace_substring(InputString, To_Replace, Replace_With, OutputString) :-
	( append([Front, To_Replace, Back], InputString)
		-> append([Front, Replace_With, Back], OutputString)
		; OutputString = InputString
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% 3. Parse input into possible plans %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Each plan_cost_line contains
%   at(Time, Action)
%   incurs_cost(Time, Metric, Degree)
% First, assign a relational ID to each plan
% Then generate for each:
%   plan_steps(ID, [...], Number)
%   plan_cost(ID, Metric, Degree, Number)

parse_input :-
	plan_cost_line(Line),
	!,
	generate_ID(ID),
	bind_steps(ID, Line),
	bind_counts(ID, Line),
	retractall(plan_cost_line(Line)),
	parse_input.

parse_input :-
	not(plan_cost_line(_)).
	
generate_ID(ID) :-
	newID(ID),
	ID2 is ID+1,
	retractall(newID(ID)),
	asserta(newID(ID2)).

bind_steps(ID, Line) :-
	findall(N, (N = at(_Time, _Action), member(N, Line)), Ns),
	sort(Ns, PlanSteps),
	length(PlanSteps, L),
	assert(plan_steps(ID, PlanSteps, L)).

bind_counts(ID, Line) :-
	findall(N, (N = incurs_cost(_T, _Metric, _Degree), member(N, Line)), CostsIncurred),
	cost_metrics(Metrics),
	cost_degrees(Degrees),
	sort_plan_costs(ID, CostsIncurred, Metrics, Degrees).

sort_plan_costs(_, _, [], _) :- !.
sort_plan_costs(ID, CostsIncurred, [Metric|MTail], Degrees) :-
	recursive_sort_plan_costs(ID, CostsIncurred, Metric, Degrees),
	!,
	sort_plan_costs(ID, CostsIncurred, MTail, Degrees).

recursive_sort_plan_costs(_, _, _, []) :- !.
recursive_sort_plan_costs(ID, CostsIncurred, Metric, [Degree|DTail]) :-
	findall(N, (N = incurs_cost(_T, Metric, Degree), member(N, CostsIncurred)), List),
	length(List, L),
	assert(plan_cost(ID, Metric, Degree, L)),
	!,
	recursive_sort_plan_costs(ID, CostsIncurred, Metric, DTail).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% 4. Evaluating plans %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

evaluate_by_function(1) :- !.
evaluate_by_function(PID) :-
	PlanID is PID - 1,
	evaluate_func(PlanID, Score),
	asserta(overall_cost(PlanID, Score)),
	evaluate_by_function(PlanID).

% Overall cost function determining plan quality
% The example overall cost function given here, somewhat arbitrarily, sums additive time and energy with multiplicative risk divided by plan length.
% TODO: % evaluate_func should be expanded so that a user can easily specify it ...
evaluate_func(PlanID, ReturnScore) :-
	sub_function(additive, time, PlanID, N1),
	sub_function(additive, energy, PlanID, N2),
	sub_function(multiplicative, risk, PlanID, N3),
	sub_function(plan_length, PlanID, N4),
	Risk is N3/N4,
	ReturnScore is N1 + N2 + Risk.

user_weight(risk, 0.1).

user_weight(energy, 1).

user_weight(time, 2).

% Default subfunctions:
% 1. An additive function for costs, according to some metric (e.g. time/energy), of each action in the specified plan
% 2. A multiplicative function for costs, according to some metric (e.g. risk), of each action in the specified plan
% 3. Possible symbolic costs for actions can be reduced to numeric scores {4, 5, 6, 7, 8} in the overall function
% 4. A user can set WEIGHTS for the metrics {time, energy, risk}

sub_function(plan_length, PlanID, Return) :-
	findall(Z, plan_steps(PlanID, _Act, Z), Zs),
	sum_list(Zs, Return).

sub_function(mean_plan_length, _PlanID, Return) :-
	findall(Z, plan_steps(_X, _Y, Z), Zs),
	length(Zs, Plans),
	sum_list(Zs, Total),
	Return is Total/Plans.

sub_function(additive, Metric, PlanID, Return) :-
	user_weight(Metric, Weight),
	cost_degrees(Costs),
	recursive_cost_sum(PlanID, Costs, Metric, Weight, 0, Return).

sub_function(multiplicative, Metric, PlanID, Return) :-
	user_weight(Metric, Weight),
	cost_degrees(Costs),
	recursive_cost_mult(PlanID, Costs, Metric, Weight, 0, Return).

sub_function(numeric_value, very_low, _PlanID, Return) :-
	Return = 4.

sub_function(numeric_value, low, _PlanID, Return) :-
	Return = 5.

sub_function(numeric_value, moderate, _PlanID, Return) :-
	Return = 6.

sub_function(numeric_value, high, _PlanID, Return) :-
	Return = 7.

sub_function(numeric_value, very_high, _PlanID, Return) :-
	Return = 8.

recursive_cost_sum(_, [], _, _, Return, Return).
recursive_cost_sum(PlanID, [Cost|Tail], Metric, Weight, Current, Return) :-
	sub_function(numeric_value, Cost, PlanID, N),
	plan_cost(PlanID, Metric, Cost, Number),
	Add is Number * N,
	New is Current + Add,
	!,
	recursive_cost_sum(PlanID, Tail, Metric, Weight, New, Return).

recursive_cost_mult(_, [], _, _, Return, Return).
recursive_cost_mult(PlanID, [Cost|Tail], Metric, Weight, Current, Return) :-
	sub_function(numeric_value, Cost, PlanID, N),
	plan_cost(PlanID, Metric, Cost, Number),
	Add is Number * N,
	(Add < 0.0000001 -> New is Current ; New is Current * Add),
	!,
	recursive_cost_mult(PlanID, Tail, Metric, Weight, New, Return).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% 5. Select a plan %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

select_plan :-
	evaluate_all_plans,
	choose_best_plan.

evaluate_all_plans :-
	newID(X),
	evaluate_by_function(X).
	
% Find plans with lowest overall_cost. Choose the shortest plan from amongst those. Set selected(ID).

choose_best_plan :-
	overall_cost(PlanID, Score),
	not((overall_cost(PlanID2, Score2), Score2 < Score)), % No plan with lower score
	not((overall_cost(PlanID2, Score), plan_steps(PlanID2, _, N2), plan_steps(PlanID, _, N1), N2 < N1)), % No shorter plan with same score
	!, % Cut: Just find first plan that meets criteria
	assert(selected(PlanID)).

worst_plan_cost(Score) :-
	overall_cost(_Pl, Score),
	not((overall_cost(_Pl2, Score2), Score2 > Score)), % No plan with higher score
	!.

mean_plan_cost(Score) :-
	findall(Num, overall_cost(_ID, Num), List),
	sum_list(List, Sum),
	length(List, L),
	Score is Sum / L,
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% 6. Print results %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_results(OutFile) :-
	% 1. Set output
	retractall(output_file(_)),
	asserta(output_file(OutFile)),
	% 2. Find information to print
	open(OutFile, write, Stream),
	selected(PlanID),
	newID(I),
	NumberOfPlans is I-1,
	sub_function(mean_plan_length, _, AveragePlanLength),
	worst_plan_cost(HighestPlanCost),
	mean_plan_cost(AveragePlanCost),
	% 3. Print summary data: number of plans, average plan length, average plan cost, lowest cost, highest cost
	write(Stream, '\n---------------------------------------'),
	write(Stream, '\n----- -----     Summary     ----- -----'),
	write(Stream, '\n* Number of plans: '),
	write(Stream, NumberOfPlans),
	write(Stream, '\n* Average plan length: '),
	write(Stream, AveragePlanLength),
	write(Stream, ' actions'),
	write(Stream, '\n* Average plan cost: '),
	write(Stream, AveragePlanCost),
	write(Stream, '\n* Highest plan cost: '),
	write(Stream, HighestPlanCost),
	write(Stream, '\n---------------------------------------'),
	write(Stream, '\n\n---------------------------------------'),
	% 4. Print steps of selected plan and its cost
	write(Stream, '\n----- -----  Selected Plan  ----- -----'),
	printPlanSteps(Stream, PlanID, 'the selected plan'),
	write(Stream, '\n'),
	% 5. Print selected plan's component costs
	printPlanCosts(Stream, PlanID),
	write(Stream, '\n---------------------------------------'),
	% 6. Print steps of every plan and their scores
	write(Stream, '\n\n---------------------------------------'),
	write(Stream, '\n----- -----    All Plans    ----- -----'),
	write(Stream, '\n'),
	printAllPlanSteps(Stream),
	close(Stream).

printAllPlanSteps(Stream) :-
	newID(ID),
	printAllPlanStepsRec(Stream, ID).

printAllPlanStepsRec(_, 1) :- !.
printAllPlanStepsRec(Stream, ID) :-
	PlanID is ID-1,
	printPlanSteps(Stream, PlanID, 'this plan'),
	write(Stream, '\n'),
	printAllPlanStepsRec(Stream, PlanID).

printPlanSteps(Stream, PlanID, String) :-
	plan_steps(PlanID, List, Count),
	write(Stream, '\n'),
	write(Stream, Count),
	write(Stream, ' steps in '),
	write(Stream, String),
	write(Stream, ' with overall cost '),
	printPlanMainCost(Stream, PlanID),
	write(Stream, ': '),
	write_term(Stream, List, [max_depth(50)]). % Print all steps even for long plans

printPlanMainCost(Stream, PlanID) :-
	overall_cost(PlanID, Score),
	write(Stream, Score).

printPlanCosts(Stream, PlanID) :-
	write(Stream, '\nComponent costs of selected plan: '),
	printPlanCostsRecursive(Stream, PlanID).

printPlanCostsRecursive(Stream, PlanID) :-
	plan_cost(PlanID, Metric, Degree, Count),
	Count > 0,
	write(Stream, '\n'),
	write(Stream, Count),
	((Count == 1) ->
		write(Stream, ' plan step had ')
		;
		write(Stream, ' plan steps had ')
	),
	write(Stream, Degree),
	write(Stream, ' cost according to metric:'),
	write(Stream, Metric),
	write(Stream, '.'),
	fail.
printPlanCostsRecursive(_, _) :- !.
