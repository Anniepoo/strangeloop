:- module(workshop, []).
/** <module> Orientation page for the workshop

*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
% html_resource inclusion
:- use_module(library(http/html_head)).
:- use_module(library(http/http_parameters)).


:- dynamic user:section_done/1.

:- http_handler(root(sectiondone), done_handler, [id(sectiondone)]).

%%	done_handler(+Request:request) is det
%
%	handle the 'Done' button
%
done_handler(Request) :-
	http_parameters(Request,
	     [
		    section(Name, [])
	     ]),
	asserta(user:section_done(Name)),
	http_redirect(moved_temporary, location_by_id(workshop), Request).

:- http_handler(root(sectionundone), undone_handler, [id(sectionundone)]).

%%	undone_handler(+Request:request) is det
%
%	handle the 'Undo' button
%
undone_handler(Request) :-
	http_parameters(Request,
	     [
		    section(Name, [])
	     ]),
	retractall(user:section_done(Name)),
	http_redirect(moved_temporary, location_by_id(workshop), Request).

:- http_handler(root(restart), restart_handler, [id(restart)]).

restart_handler(Request) :-
	 http_parameters(Request,
		[ restart(Restart, [between(0,180), default(-1)])
		]),
	restart_time(Restart),
	http_redirect(moved_temporary, location_by_id(workshop), Request).



:- http_handler(root(workshop), workshop_page, [id(workshop)]).

/**  workshop_page(+Request:request)// is det

    handler pred for the page with the workshop lesson plan

*/
workshop_page(_Request) :-
	reply_html_page(
	    title('Strangeloop 2013'),
	    [
		\html_requires('/help/source/res/pldoc.css'),
		\html_requires('/help/source/res/pldoc.js'),
		\html_requires('/f/workshop.css'),
		h1('Strangeloop 2013'),
		h2('Real Development Boot Camp in '),
		h2(\img_fix([src('/f/swipl.png')], [])),
		\sections,
		hr([]),
		\start_form,
		hr([])
	    ]).

start_form -->
	html([
	    form(action('/restart'), [
		 label(for(restart), 'Restart this many minutes into workshop:'),
		 input([type(number), maxlength(3), min(0), max(180),
			name(restart), value(0)], []),
		 input([type(submit), name(ok), value(restart)], [])
		 ])
	]).

%%	sections// is det
%
%	generates the list of sections in the workshop
%
sections -->
	{
	    time_remaining(Time),
	    MissionTime is floor(180 - Time),
            bagof(Name, A^B^section_head(Name, A, B), Names),
            time_restricted_unfinished_sections(Time, Names, WeWillCover, _),
            !
        },
	html([p([\wall_time, ' time remains ', Time])]),
	section(MissionTime, Names, WeWillCover, false).


%%	section(+StartAt:time, +Names:list, +WeWillCover:list,
%	+CurrentFound:atom)// is det
%
%	generate a set of sections recursively
%
%	@arg StartAt Mission time at which we start section
%	@arg Names   List of sections
%	@arg WeWillCover  List of those sections we'll cover
%			  we might run out of time
%	@arg CurrentFound if false we haven't found the current section
%	                  if true, we already have
%
% we will do this case
section(StartAt, [Name | T], WeWillCover, CurrentFound) -->
	{
	   member(Name, WeWillCover),
	   section_head(Name, Label, File),
           mission_wall(StartAt, Wall),
	   timing(Name, Timing),
           NextStartsAt is StartAt + Timing,
           atomic_list_concat(['HTTPrequest(\'/help/source/edit?file=', File, '\')'], AFile),
           (   bagof(Desc, subsection(Name, Desc), Subsections) ->
               true
           ;
               Subsections = []
           ),
           (   CurrentFound = false ->
	       SectionClass = [section, willdo, current]
	   ;
	       SectionClass = [section, willdo]
	   )
        },
	html(div(class(SectionClass), [
		     p([\done_section(Name),
			span(class(wall), Wall),
			span(class(sectionlabel), Label),
			Timing, ' mins',
			a([class(editlink), href('#'), onClick(AFile)], 'See The File')
		       ]),
		     ul(\subsections(Subsections)),
		     \section_images(Name, [])
		 ])
	),
	section(NextStartsAt, T, WeWillCover, true).

% done case
section(StartsAt, [Name | T], WeWillCover, CurrentFound) -->
	{
           user:section_done(Name),
	   section_head(Name, Label, File),
	   timing(Name, Timing),
           atomic_list_concat(['HTTPrequest(\'/help/source/edit?file=', File, '\')'], AFile),
           (   bagof(Desc, subsection(Name, Desc), Subsections) ->
               true;
               Subsections = []
           )
        },
	html(div(class([section, done]), [
		     p([\done_section(Name), span(class([sectionlabel, done]), Label),
			     Timing, ' mins',
			     a([class(editlink), href('#'), onClick(AFile)], 'See The File')
			    ]),
		     ul(\subsections(Subsections)),
		     \section_images(Name, [])
		 ])
	),
	section(StartsAt, T, WeWillCover, CurrentFound).

% won't do case
section(StartsAt, [Name | T], WeWillCover, CurrentFound) -->
	{
	   section_head(Name, Label, File),
	   timing(Name, Timing),
           atomic_list_concat(['HTTPrequest(\'/help/source/edit?file=', File, '\')'], AFile),
           (   bagof(Desc, subsection(Name, Desc), Subsections) ->
               true;
               Subsections = []
           )
        },
	html(div(class([section, notime]), [
		     p([\done_section(Name),
			span(class([sectionlabel, notime]), Label),
			     Timing, ' mins',
			     a([class(editlink), href('#'), onClick(AFile)], 'See The File')
			    ]),
		     ul(\subsections(Subsections)),
		     \section_images(Name, [])
		 ])
	),
	section(StartsAt, T, WeWillCover, CurrentFound).
section(_, [], _, _) --> [].

%%	subsections(+List:list)// is det
%
%	generates a list of the subsections from the arglist
%
subsections([]) --> [].
subsections([H|T]) -->
	html(li(H)),
	subsections(T).

%%	section_images(+Name:atom, +Done:list)// is det
%
section_images(Name, Done) -->
	{
	   % suprise - we're in Prolog, this is a loop!
           section_image(Name, File, Alt),
	   \+ member(File, Done)
        },
	html(\img_fix([src('/f/'+File), class(illustration), alt(Alt)], [])),
	section_images(Name, [File | Done]).
section_images(_, _) --> [].

show_jan_bug.

img_fix(Attribs, Contents) -->
	{
           show_jan_bug
        },
	html(img(Attribs, Contents)).

img_fix(Attribs, Contents) -->
	{
    % Hack! html_write:attributes poorly exposed
	   member(src(SrcPath+SrcFile), Attribs),
           random(N),
	   format(atom(UniqSrc), '~w~w?r=~w', [SrcPath, SrcFile, N]),
	   select(src(_), Attribs, src(UniqSrc), NewAttribs)
        },
	    html(img(NewAttribs, Contents)).
img_fix(Attribs, Contents) -->
	{
            member(src(Src), Attribs),
            random(N),
	    format(atom(UniqSrc), '~w?r=~w', [Src, N]),
	    select(src(Src), Attribs, src(UniqSrc), NewAttribs)
        },
	    html(img(NewAttribs, Contents)).
img_fix(Attribs, Contents) -->
	    html(img(Attribs, Contents)).

done_section(Name) -->
	{
	  \+ user:section_done(Name)
        },
	html([
	    form([class(doneform), action('/sectiondone')], [
		 input([type(hidden), name(section), value(Name)], []),
		 input([type(submit), name(ok), value(done)], [])
		 ])
	]).
done_section(Name) -->
	{
	  user:section_done(Name)
        },
	html([
	    form([class(doneform), action('/sectionundone')], [
		 input([type(hidden), name(section), value(Name)], []),
		 input([type(submit), name(ok), value('undo')], [])
		 ])
	]).


%%	mission_wall(+T:number, +Wall:atom)// is det
%
%	convert time in mission time (minutes since start of workshop)
%	to wall clock time starting at 9am
%
mission_wall(T, Wall) :-
	Hours is T // 60 + 9,
	Mins is T mod 60,
	format(atom(Wall), '~w:~|~`0t~d~2+', [Hours, Mins]).

%%	wall_time// is det
%
%	inclusion displaying wall time
wall_time -->
	{
	     get_time(Now),
             latest_start_time(Start),
             Mission is floor(Now - Start) // 60,
	     mission_wall(Mission, Wall)
        },
	html([
	    span(Wall)
	]).

%%	time_restricted_unfinished_sections(+TimeAvailable:number,
%	+Sections:list, -BestSections:list, -Score:number) is det
%
%	from among Sections select BestSections s.t. Score is maximized
%	and TimeAvailable is not exceeded and we aren't doing any that
%	have already been done per user:section_done
%
time_restricted_unfinished_sections(TimeAvailable, Sections, BestSections, Score) :-
	not_done_sections(Sections, NotDoneSections),
	time_restricted_sections(TimeAvailable, NotDoneSections, BestSections, Score).

not_done_sections([], []).
not_done_sections([H|T], NotDoneTail) :-
	user:section_done(H),
	!,
	not_done_sections(T, NotDoneTail).
not_done_sections([H|T], [H|NotDoneTail]) :-
	not_done_sections(T, NotDoneTail).


%%	time_restricted_sections(+TimeAvailable:number, +Sections:list,
%	-BestSections:list, -Score:number) is det
%
%	from among Sections select BestSections s.t. Score is maximized
%	and TimeAvailable is not exceeded
%

% base case - no more sections
time_restricted_sections(_, [], [], 0).

% we have enough time to do all the rest -
% checking for this speeds up algorithm
time_restricted_sections(TimeAvailable, Sections, Sections, TotalScore) :-
	total_time(Sections, 0, TotalTime),
	TotalTime =< TimeAvailable,
	total_score(Sections, 0, TotalScore).

% not enough time to do this section by itself
time_restricted_sections(TimeAvailable, [H|T], BestSections, Score) :-
	timing(H, ThisTime),
	TimeAvailable < ThisTime, !,
	time_restricted_sections(TimeAvailable, T, BestSections, Score).

% otherwise try with and without H and choose the better
time_restricted_sections(TimeAvailable, [H|T], BestSections, Score) :-
	timing(H, ThisTime),
	priority(H, ThisScore),
	RemainingTime is TimeAvailable - ThisTime,
	time_restricted_sections(TimeAvailable, T, BestNoSections, NoScore),
	time_restricted_sections(RemainingTime, T, BestYesSections, YesTScore),
	YesScore is YesTScore + ThisScore,
	(   YesScore > NoScore
	->
	    Score = YesScore,
	    BestSections = [H | BestYesSections]
	;
	    Score = NoScore,
	    BestSections = BestNoSections
	).

%%	total_time(+List:list, +Seed:number, -Time:number) is det
%
%	 sum the times associated with the names in List,
%	 Usually pass in 0 as Seed
%
total_time([], T, T).
total_time([H|T], InTime, OutTime) :-
	timing(H, ThisTime),
	NT is InTime + ThisTime,
	total_time(T, NT, OutTime).

%%	total_score(+List:list, +Seed:number, -Score:number) is det
%
%	@arg List   list of section names
%	@arg Seed   seed to start from  (usually 0)
%	@arg Score  sum of the priorities of items in list
%
total_score([], S, S).
total_score([H|T], InScore, OutScore) :-
	priority(H, Score),
	NS is Score + InScore,
	total_score(T, NS, OutScore).

:- dynamic user:start_time/1.

user:start_time(1379518200).

%%	restart_time(+Time:int) is det
%
%	if -1 do nothing
%	otherwise restart timer at this many minutes into talk
%
restart_time(-1).
restart_time(T) :-
	get_time(Now),
	Start is Now - T * 60,
	retractall(user:start_time(_)),
	asserta(user:start_time(Start)).
restart_time(_).

%%	time_remaining(-Time) is det
%
%	time remaining in the workshop in minutes
%	prior to start reports 180
%	after end reports 0
%
time_remaining(Time) :-
	get_time(Now),
	latest_start_time(Start),
	Now < Start,
	Time = 180.
time_remaining(Time) :-
	get_time(Now),
	latest_start_time(Start),
	End is Start + 180 * 60,
	Now > End,
	Time = 0.
time_remaining(Time) :-
	get_time(Now),
	latest_start_time(Start),
	Time is (Start + 180 * 60 - Now) / 60.

%%	latest_start_time(-Time:int) is det
%
%	return only the most recently set time
%
latest_start_time(Time) :-
	user:start_time(Time), !.

:- discontiguous
	section_head/3,
	timing/2,
	priority/2,
	subsection/2,
	section_image/3.

%%	section_head(-Name:atom, -Desc:atom, -File:atom) is det
%
section_head(intro, 'Workshop Organization', 'workshop.pl').
timing(intro, 5).
priority(intro, 100).
subsection(intro, 'Look at code for a server').
subsection(intro, 'Play while I jabber at you').
subsection(intro, 'Feel free to interrupt, that\'s the point').
subsection(intro, 'Getting to the end on time is minor').
subsection(intro, 'workshop page').

section_head(troubleshoot,
	     'Get everybody running',
	     'debug.pl').
%%	timing(-Name:atom, -Minutes:number) is det
timing(troubleshoot, 15).
%%	priority(-Name:atom, -Score:number) is det
priority(troubleshoot, 100).

section_head(starting_the_server,
	     'The Multithreaded Server',
	     'debug.pl').
timing(starting_the_server, 10).
priority(starting_the_server, 50).

%%	subsection(-Name:atom, -Desc:atom) is det
subsection(starting_the_server, 'We are our own web server').
subsection(starting_the_server, 'The \'usual project files\' and load sequence').
subsection(starting_the_server, 'We piggyback the pldoc server during dev mode').
subsection(starting_the_server, 'The http_server call').
%%	section_image(-Name:atom, -File:atom, -Desc:atom) is det
section_image(starting_the_server,
	      'organization.png',
	      'Overall Server Organization').

section_head(pldoc_intro,
	     'Intro to PLDoc',
	     '/help/source/').
timing(pldoc_intro, 5).
priority(pldoc_intro, 10).

section_head(environment_setup,
	     'Making A Comfy Environment',
	     'debug.pl').
timing(environment_setup, 10).
priority(environment_setup, 25).
subsection(environment_setup, portray_text).
subsection(environment_setup, 'Firing up editor and opening pages automatically').
subsection(environment_setup, 'Some editor tricks').
subsection(environment_setup, 'Prolog Navigator, other IDE tools').
subsection(environment_setup, 'Abstract URI and file paths').

section_head(handlers_intro,
	     'Introduction to Handlers',
	     'simple_handlers.pl').
timing(handlers_intro, 15).
priority(handlers_intro, 75).
subsection(handlers_intro, 'Handler basics').
subsection(handlers_intro, 'Generating the reply by printing').
subsection(handlers_intro, 'the Howdy handlers').
subsection(handlers_intro, challenge('Say hi to your partner')).

section_head(break1,
	    'Break 1',
	    'workshop.pl').
timing(break1, 5).
priority(break1, 100).

section_head(html_generation_path,
	     'The HTML Generation Sequence',
	     'html_handlers.pl').
timing(html_generation_path, 10).
priority(html_generation_path, 50).
section_image(html_generation_path,
	      'html_path.png',
	      'The sequence of events for HTML generation').

section_head(html_generation,
	     'HTML Generation',
	     'html_handlers.pl').
timing(html_generation, 20).
priority(html_generation, 80).
subsection(html_generation, 'Two Camps - templates and homoiconic').
subsection(html_generation, 'DCGs').
subsection(html_generation, 'Termerized HTML syntax').
subsection(html_generation, 'Exercise').

section_head(html_inclusions,
	     'Inclusions',
	     'html_handlers.pl').
timing(html_inclusions, 15).
priority(html_inclusions, 50).
subsection(html_inclusions, 'Basics').
subsection(html_inclusions, 'Modules').

section_head(break2,
	     'Break 2',
	     'workshop.pl').
timing(break2, 5).
priority(break2, 100).

section_head(html_qq,
	     'HTML Quasiquotes',
	     'html_handlers.pl').
timing(html_qq, 10).
priority(html_qq, 15).

section_head(mailman,
	     'Mailman',
	     'mailman.pl').
timing(mailman, 8).
priority(mailman, 25).

section_head(serving_files,
	     'Ids and Serving Files',
	     'fancy_handlers.pl').
timing(serving_files, 8).
priority(serving_files, 20).

section_head(styling,
	     'Styling',
	     'styling_handlers.pl').
timing(styling, 5).
priority(styling, 25).

section_head(parameter_handling,
	     'Parameter Handling',
	     'handle_params.pl').
timing(parameter_handling, 7).
priority(parameter_handling, 25).

section_head(resources,
	     'Resource Inclusion',
	     'resourcedemo.pl').
timing(resources,  15).
priority(resources, 35).

section_head(sessions,
	     'Sessions',
	     'sessions.pl').
timing(sessions, 10).
priority(sessions, 10).

section_head(javascript,
	     'Javascript',
	     'clippy_demo.pl').
timing(javascript, 5).
priority(javascript, 5).

section_head(conclusion,
	     'Conclusion',
	     'workshop.pl').
timing(conclusion, 15).
priority(conclusion, 100).

