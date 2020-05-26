:- module(clippy, [clippy//1]).
/** <module> Utils for working with Clippy-js

Clippy-js is from https://www.smore.com/clippy-js

   This module is slightly modified from the weblog
   pack.

*/
:- use_module(library(http/html_head)).

% The resources below assume the css comes from the path
% /css/blahblah and js comes from /js/blahblah
% The file handlers for those are set up by swipl, but
% we want them to also look for the files in our single assets directory
% so we're adding a path to the search path
%
% In a real application, we'd do this in strangeloop.pl
%
user:file_search_path(css, assets).
user:file_search_path(js, assets).

% define some resources
%
% In a real application, we'd do this in strangeloop.pl
%
:-html_resource(jquery, [virtual(true),
       requires(['http://code.jquery.com/jquery-1.9.1.js'])]).
:-html_resource(clippy, [virtual(true),
			 requires([jquery, css('clippy.css'), js('clippy.min.js')])
			]).

:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/js_write)).

:- meta_predicate clippy(1, ?, ?).

%%	clippy(+Generator:goal)// is nondet
%
%	Makes clippy appear on screen, using clippy-js
%	from https://www.smore.com/clippy-js
%
%	generator is an arity 1 goal queried with
%	an argument of form
%
%	* character(Char)   Character is one of Clippy, Merlin,
%	  Rover, or Links  (default clippy)
%
%       * id(ID) ID is the ID of the agent (default agent1236742)
%
%	this just brings the character up. You have to control them
%	yourself.
%
clippy(Generator) -->
	{
             (	  call(Generator, character(Char)) ;
                  Char = 'Clippy'
             ),
	     member(Char , ['Clippy', 'Merlin', 'Rover', 'Links'])
        },
	html([
	    \html_requires(clippy),
	    \js_script({| javascript(Char) ||
var agent;
$(function() {
    clippy.load(Char, function(agent1236742) {
	agent = agent1236742;

        // Do anything with the loaded agent
        agent.show();
    });
});
|}
		       )]).
