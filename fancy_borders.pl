:- module(fancy_borders, [fancy_border//2]).
/** <module> Various fancy borders

     Imagine this offers a bunch of types of fancy borders you can put
     around html elements, some of which can't be made with just css
     border
*/

:- use_module(library(http/html_write)).

% a powerful spell to ward off module issues
% forces module resolution before call
% (and makes sure we get syntax coloring  8cD   )
%
:- html_meta fancy_border(+, html, ?, ?).

%%	fancy_border(+Type:atom, +Contents:html)// is det
%
%	wraps Contents in a fancy border whose appearance depends
%	on Type
%
%	* dot_madness  -  a bold set of green dots
%
%	* boring - an elegant black line
%
fancy_border(dot_madness, Contents) -->
	html(div(style('border: dotted 1.5em #008800'), Contents)).
fancy_border(boring, Contents) -->
	html(div(style('border: solid 1px #000000'), Contents)).
