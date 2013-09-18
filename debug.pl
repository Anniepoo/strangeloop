:- module(debug, []).
/** <module> Dev mode starter file

    Consult this file to start the system in dev mode

*/

% Make sure we're on a reasonable version
%

%%	reasonable_version is nondet
%
%	succeeds if our version is 6.4.1 or better
%
reasonable_version :-
	current_prolog_flag(version_data, swi(Major, _, _, _)),
	Major > 6.
reasonable_version :-
	current_prolog_flag(version_data, swi(6, Minor, _, _)),
	Minor > 3.
reasonable_version :-
	current_prolog_flag(version_data, swi(6, 3, Patch, _)),
	Patch > 18.

check_version :- reasonable_version, !.
check_version :-
      current_prolog_flag(version_data, swi(Major, Minor, Patch, _)),
      format('OOOPS - you need swipl version 6.4.1 or better, you are on ~w.~w.~w~n',
	[Major, Minor, Patch]).

:- check_version.

%%	force_right_directory
%
%	Change the working directory to the directory this file
%	loaded from so we don't have weird relative path issues
%
force_right_directory :-
	source_file(check_version, File),
	file_directory_name(File, Dir),
	working_directory(_, Dir).

:- force_right_directory.

% Make it easier to read 'codes' style strings
:- portray_text(true).

% pldoc is SWI-Prolog's equivilent of doxygen
% pldoc is served at / by default. We need to
% move pldoc's URI path so it doesn't interfere with the main
% application. Each SWI-Prolog process has a single global
% URI space for dispatch.

% abstract URI paths allow us to disconnect the served URI path
% from the name we refer to it by. By default, pldoc('foo/bar') maps
% to /foo/bar. We'll move pldoc to /help/source, so pldoc('foo/bar')
% will serve from /help/source/foo/bar

% First we import the abstract path stuff
:- use_module(library(http/http_path)).

% Now we define a new clause for the *multifile* predicate
% http:location/3. Default priority is 0, we use a higher
% priority to override pldoc's default location
% We define pldoc in terms of [system defined] root.
%
http:location(pldoc, root('help/source'), [priority(10)]).

% load our application server
% We start it first
% so it doesn't collide with pldoc
:-ensure_loaded(load).

% we don't actually start the server ourselves, in dev mode
% we piggyback on the pldoc server
%
% :- strangeloop_server.

% Now we can start pldoc. This starts our application server
% as well, a workaround for one server per process
:- doc_server(7777).

% Nice thing about SWI-Prolog, the interface to most
% development environment is fairly simple, so it's practical
% to set your environment for your own convenience. Here, we
% launch our first handler
:-www_open_url('http://localhost:7777/').

% and our pldoc home page
:-www_open_url('http://localhost:7777/help/source/').

% and the workshop page
:-www_open_url('http://localhost:7777/workshop').


% and bring up a module in the editor
:- edit('debug.pl').

% open the navigator and the cross referencer
% put it in module user so programmer doesn't have to
% use a module name
user:open_tools :- prolog_ide(open_navigator),
              prolog_ide(xref).

