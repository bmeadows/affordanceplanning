# Affordance-aware plan generation and selection

Built on platforms SPARC and SWI-Prolog.

robaff_revised.sp is the plan generation component. To run it:

   java -jar sparc.jar robaff_revised.sp -A > plans_out.txt

selector.pl is the plan selection component. To run it:

   run('plans_sample.txt', 'out.txt').
