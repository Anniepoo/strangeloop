:- module(styling_handlers, []).
/** <module> Demos of styling hooks, mailman, and resources

*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).


%
% How can we include fixed elements in our pages?
% So far we've used reply_html_page/2, but we can pass
% an optional 'styling argument'

% we define a new clause for the multifile hook predicate
% user:body with our constant items
%
user:body(strangeloop_style, Body) -->
        html(body([ div(id(top), h1('The Simple Web Page Site')),
                    div(id(content), Body)
                  ])).
% we could define user:head if we wanted as well
%

:- http_handler(root(styled), styled , [id(styled)]).

styled(_Request) :-
    reply_html_page(
        strangeloop_style,   % define the style of the page
       [title('Howdy')],
        p('this page has style')).

/*
    So, you know that awkward moment when you realize you could
    generate a second chunk of html at this moment, along with whatever
    we're making, but nooo... we're not at that point.
    Or those awkward 'stick this in the head, this at the bottom, this
    right below the element, blah blah' complicatedjavascript driven
    widgets that are always a nuisance?

    Mailman to the rescue

*/
:- http_handler(root(mailman), mailman_demo, [id(mailman)]).

%%	mailman_demo(+Request:request) is det
%
%	demonstration of the mailman facility

mailman_demo(Request) :-
	reply_html_page(
	   title('Howdy'),
	   [\page_content(Request)]).

page_content(_Request) -->
	html(
	   [
	    h1('Demonstrating Mailman'),
	    %
	    % we generate both sets of buttons here
	    % and send them to...
	    div(\nav_bar),
	    p('The body goes here'),
	    %
	    % down here...
	    div(\html_receive(bottom_nav))
	   ]).

nav_bar -->
	{
	    % pretend this is painful
	    findall(Name, nav(Name, _), ButtonNames),
            % and this is a handy place to make all the buttons
	    maplist(as_top_nav, ButtonNames, TopButtons),
	    maplist(as_bottom_nav, ButtonNames, BottomButtons)
	},
	html([\html_post(bottom_nav, BottomButtons) , TopButtons]).


nav('Home', '/home').
nav('About', '/about').
nav('Recipes', '/recipes').

as_top_nav(Name, a([href=HREF, class=topnav], Name)) :-
	nav(Name, HREF).

as_bottom_nav(Name, a([href=HREF, class=bottomnav], Name)) :-
	nav(Name, HREF).

% very useful - the default page generation includes an
% html_receive(head) in the head



%%	TBD
%   resources
%   parameters
%   sessions
%   json
%   backend
%
