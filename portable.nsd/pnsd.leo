<?xml version="1.0" encoding="UTF-8"?>
<leo_file>
<leo_header file_format="2" tnodes="0" max_tnode_index="26" clone_windows="0"/>
<globals body_outline_ratio="0.5">
	<global_window_position top="0" left="42" height="683" width="974"/>
	<global_log_window_position top="0" left="0" height="0" width="0"/>
</globals>
<preferences allow_rich_text="0">
</preferences>
<find_panel_settings>
	<find_string></find_string>
	<change_string></change_string>
</find_panel_settings>
<vnodes>
<v t="T1"><vh>Directories</vh></v>
<v t="T2"><vh>Notes</vh>
<v t="T3"><vh>Test Plan</vh></v>
<v t="T4" a="E"><vh>@file NsTcl</vh></v>
</v>
<v t="T5"><vh>lib</vh>
<v t="T6" a="C"><vh>&lt;&lt;copyright&gt;&gt;</vh></v>
</v>
<v t="T7"><vh>src</vh>
<v t="T8"><vh>@file-nosent pnsd-fastcgi.tcl</vh>
<v t="T6" a="C"><vh>&lt;&lt;copyright&gt;&gt;</vh></v>
</v>
<v t="T9"><vh>@file-nosent pnsd-init.tcl</vh>
<v t="T6" a="C"><vh>&lt;&lt;copyright&gt;&gt;</vh></v>
</v>
<v t="T10" a="E"><vh>@file-nosent ns_misc.tcl</vh>
<v t="T6" a="C"><vh>&lt;&lt;copyright&gt;&gt;</vh></v>
</v>
<v t="T11" a="E"><vh>@file pnsd-cgi-bootstrap.tcl</vh></v>
<v t="T12"><vh>@file pnsd-bootstrap.tcl</vh></v>
<v t="T13" a="E"><vh>@file-nosent ns_conn.tcl</vh>
<v t="T6" a="C"><vh>&lt;&lt;copyright&gt;&gt;</vh></v>
</v>
</v>
<v t="T14"><vh>doc</vh>
<v t="T15"><vh>Driving Principals</vh></v>
<v t="T16"><vh>@file-nosent INSTALL.CGI</vh></v>
<v t="T17"><vh>@file-nosent LICENSE</vh></v>
<v t="T18"><vh>@file-nosent INSTALL.TCLHTTPD</vh></v>
<v t="T19"><vh>@file-nosent README</vh></v>
<v t="T20"><vh>@file-nosent TODO</vh></v>
<v t="T21"><vh>@file-nosent RELEASE</vh></v>
<v t="T22"><vh>@file-nosent INSTALL</vh></v>
<v t="T23"><vh>@file-nosent INSTALL.FCGI</vh></v>
</v>
<v t="T24"><vh>announce</vh></v>
<v t="T25"><vh>openacs-4.5</vh></v>
<v a="E"><vh>Bugs</vh></v>
<v a="E"><vh>TODO</vh>
<v t="T26" a="V"><vh>installer</vh></v>
</v>
</vnodes>
<tnodes>
<t tx="T1">
(LAPTOP)

Directories

c:\web\openacs-4
c:\web\openacs-4.bak


CONFIG FILE
nsd -f -t c:\nsd4\openacs4.tcl 
(points to acs4 on 192.168.101.106 7.3 (Debian Instance)
</t>
<t tx="T2"></t>
<t tx="T3">does go.tcl work?
does go-cgi.tcl work?

Have the windows paths been removed?
</t>
<t tx="T4">@path c:/cygwin/home/john/Wiki
@code

Fast NS Tcl

NsTclToDo


 3I'm getting postgres up,  just for fun.

Then I'll try to execute some commands against it


Task:  Try to load a cleverly library and call a routine
2 access the db

Sourcing things: investigate ns. calls in ACS
3 Can I load a couple libraries?  or should I just try to access the mssql database

&lt;verbatim&gt;
*Postgres Initilization:
ipc-daemon &amp;
/usr/bin/postmaster -D /usr/local/pgsql/data -i

psql -U john -d openacs

http://michael.cleverly.com/nstcl/database%5fapi.html 

&lt;/verbatim&gt;

I dropped the libpgtcl.dll from pgAccess into the tcl\bin folder as well.

Now,  where do we start in openacs?  And how do I build a script that kicks it off?

Ah yes,  /tcl/0-acs-init.tcl

At this point,  I need serious help with the nsv stuff.  Michael!!!!  I'm lame.  I think the list references aren't working correctly.  But I've gotten through several pages of code,  which is good.
Tons of stuff works,  and more will once I've got a few basics in place.
I should begin to think about writing unit tests to check out the code as it's run.  This will help me when I migrate to tequlia or other shared state.

Questions for Michael.  
1) The Postgres id barfs.  I'm not sure OpenACS can handle it.
2) Why does &lt;nop&gt;TclPro barf on the forwardcompat declaration?  Can I safely remove it?

To get TclPro to

For the first fix,  I had to make sure nsvget returned the correct arg type when getting a list

&lt;verbatim&gt;
    if { [catch { proc $pro_name $arguments $code } ] } { 
	puts  " problem defining $proc name "
    }

&lt;/verbatim&gt;

Next issue:  install plpgsql language into openacs (Add this as part of docs.)
Set up web page.

&lt;verbatim&gt;
createlang -U john plpgsql openacs   ( worked )
&lt;/verbatim&gt;

Open Issues: nsvset getting called twice (according to STDOUT).  
&lt;verbatim&gt;
Ignore proc_doc stuff for now.

I'm having an issue with pg_class.  Why hasn't this been defined yet?


Something is wrong with ns_xml.  Why isn't the pure tcl version being used?

Figure out whether parsing is working okay.
Why can't "dom" be seen by TclPro
Figure out persist flag and how to handle it.   ( delete tree in cleanup? )

I commented out a few proc's in tdom that were defined as being in there for incrtcl compatibility.  Now TclPro works again.  (But of course incrtcl is not working).

source this file... 30-xml-utils-procs.tcl
then play with 



proc db_qd_internal_load_cache {file_path} {

TEST THIS PROCEDURE (by using DOM to source it and xml helper routines.)
"db qb internal load cache"
&lt;/verbatim&gt;
This will allow you to walk through db file loading routines to see what's broken.


I should consider hacking on OpenACS,  but I'd rather not.


&lt;verbatim&gt;
I need to investigate how the current_rdbms is determined.

William Duquette object command
http://www.wjduquette.com/tcl/objects.html 

I've implemented ns_xml delete,  so my memory leaks should be slightly fewer.
Now I've got to get the bind variables working correctly.
The pg_bind.. function I proxied in must be revisited.

ad_verify install is croaking.
&lt;/verbatim&gt;

I'd like to figure out a way to not instrument a lot of the code base.

ns.pg.bind is my new bottleneck.  I've got to write a hacking type function to get this to work.  I'm worried about db optimization if I need to determine variable type.

&lt;verbatim&gt;
Err.  Another error  
[ns.set value $selection 0] should work,  but doesn't. in db_string.

I need to test sql {	select count(*) from pg.class }


ns.pg.bind $type $db -bind $bind $sql


set db [ns.db gethandle]; 
set x [ns.db 0or1row $db "select count(*) as num from pg.class"]
puts $x

returns "t10"  - weird

Now we're talking about the same error... 

db.string ... is defined in 00-database-procs.tcl,  and it is messed up I think.  I need to redefine that proc to point to nstcl's db.string

I've gotten an error parsing the .info file.  


I need to add a test for db.select
I should add a test for ...
grrr.  I want to get past apm parsing,  but debugging is painful.


db.exec is located in 00-database-procs-postgresql.tcl  and is broken.
It makes a 0or1row call into ns.misc.tcl,  which doesn't return something ns.set can handle

I really need to allow XQL loading to be on-demand if desired.  It takes a ridiculous amount of time to parse and cache all those,  and for shell scripts that just want to run a little code,  or test a bit of the API,  it's overkill.
The other option is to somehow cache these files (or that nsv) in the database... that'd be a better short term solution.

I deleted the packages I didn't think I'd be needing,  because scanning and loading takes a lot of time.

querytext should be CDATA... why not?

If you look closely at the log,  it appears as though the fullquery name is not being formed correctly.  

apm.package.url.from.key.mem.apm..package.url.from.key-
two queries are getting sucked into a single pathname.
Must fix this. (and serialize?)

No SQL is getting fed into the first place we use the 

The query loader was choking on my hack of ns.xml "name".  Now I have to figure out what fix that was for so I can patch it.
IDEA:  Query dispatcher is very lame.  I think the best thing to do from a startup and debugging perspective is to run the source tree through a reverse extractor,  to put the correct queries back into the scripts.  
Then the dispatcher code could be bypasesd directly,  startup time would be hugely decreased,  and everything would be way easier to debug.

set list.of.source.procs {ns.sourceproc apm.source template::adp.parse template::frm.page.handler rp.handle.tcl.request}

Alright.  So I have a db api acceptance test,  courtesy of donb.  I'll see if I can't make that work-- it should be easier to walk through and debug than the rest of the application (though I don't see any db.exec.pgplsql calls in it.


Thursday 9/20/02
The work on the bind function was pointless,  because it's already working.
I'd like to get the acceptance test to work,  but I might need MC's help with that.
I'm thinking seriously about removing the query dispatcher,  because it adds a lot of difficult to debug code to what I'm trying to do.

To do this:  are bind vars used anywhere?


Instead of using XML for format files,  what's wrong with a tcl-based serialization format?

I had my big a-ha moment.  the pg.bind function must return an ns.set,  not the value of the query.
Big disconnect between what I thought it should do, and what it was doing.


New line

Database driver too old... I remember this error resulted from pg.class not being able to be counted.  I've got some work to do on binding.

I'm having trouble with releasing handles... they get chewed up.
I need help rewriting db.with.handle.  I'll see if MC can come up with anything.

I will rewrite calls to db.with.handle so that they don't call it.
I will test the connection pooling code to see if I can call five queries in a row.

I can't track this down..

I'm dying somewhere in the apm 


AHA!!! query dispatcher must die again!!!

I found another location where the Oracle query was located inline, and since I was calling directly against nstcl the wrong query was invoked.

It's time for the query dispatcher to die!!!
xparse.pl in ms.oacs directory will start me out.  I have to make sure I only grab the string when it's fronted by a db. call.  Then do a find and replace on the content with the XQL version of the query.
QED

I'm successfully replacing the queries.
Next step. Walk source tree, process all files, move old files to different tree.  Test this out. (Can I always use the brackets {} ? )

All set on file parsing.  Back up old files, run parser, and see how far I now get with go.tcl.

Tuesday
I have a small bug in the QD remover that prevents empty quoted strings from working.

Fix that,  then change the overwrite call to use $file.tcl,  then delete all the xql files,  and see if the installer code runs.
Otherwise use zip file backup and try again.


Okay -&gt; I've rewritten all the tcl files to include ...

BUG! The apm-.... fixed.

BUG!  Database driver too old... I should make sure this db. command is working okay.

BUG! ns_conn not implemented.
&lt;/verbatim&gt;

Woohoo - I'm getting into page serving.


The content repository is taking forever to load.  I should test this out on Unix to see if it isn't a problem with cygwin Postgres.  Perhaps I've exhausted theconnections on the db?  It's weird,  because psql seems so responsive.

I forgot to run xparse.pl on the new dependency packages I've added.
I'm getting the following error:

&lt;verbatim&gt;

FILE:#############################################d:/projects/portable.nsd/openacs-4/packages/acs-messaging/tcl/acs-messaging-procs.tcl####################
Parse Error: 
not well-formed at line 52, column 32, byte 1852 at D:/Perl/site/lib/XML/Parser.pm line 168
&lt;/verbatim&gt;

I should trouble-shoot this to make sure the queries get imploded correctly.

FIXED:The above was fixed by using CDATA in the XQL files


NsTcl
search currently dies on the statement:
db.exec.plpgsql 
called with instantiate and mount.  I think this problem is in ns_db 0or1row.
I'm having trouble setting breakpoints and looking at procedures in TclPro,  so I'll have to use puts to debug this.

I'd also like to download an evaluation of ActiveState's release of this to see if I can get it to work.

What I need to do is create a single function that uses dml to try and create a function.
then walk through this with a debugger.

(or better yet mimic this call to db_exec_plpgsql


Did that.  create function just plain doesn't work,  maybe?


nscache documentation
http://dqd.com/~mayoff/aolserver/src/nscache//index.html


root      2176
site_node 2227

There was an error in the xparser.  packages/subsite/tcl/package-procs.tcl has some nested brackets that weren't parsed correctly by xparser,  and had to be hand edited.  I'm not sure what I'll do about this yet.

Now I'm getting the setid error.
I would like to be able to reproduce this in a script,  but I can't yet.

I was able to reproduce it -- this was a good idea.  I needed to pass in a -set flag to 0or1row to make sure I pgplsql returned a set.

QED.

Now I've got to fix the ns_cache... set up a test script to get and retrieve values

Get this to work (set it up as tcltest??? )
D:\Projects\portable.nsd\t&gt;tclsh ns_cache.tcl 

Then get ns_eval to work ... nah -&gt; it's deprecated.  I'll just comment it out.

ns_cache is writing to the db successfully,  and I've got a test script up.


I'll start with this code:

    ad_conn -reset
    ad_conn -set request [nsv_incr rp_properties request_count]
    ad_conn -set user_id 0
    ad_conn -set start_clicks [clock clicks]


memoize is not working... I need to write a few more tests to make sure that it can interoperate with ns_cache okay.
also,  do I need t

Warn : ns_conn headers not implemented yet 
ns_config called with ns/server/openacs/module/nssock Hostname
Warn : ns_conn url not fully implemented yet 
Warn : ns_conn urlc not implemented yet 
Warn : ns_conn urlv not implemented yet 
Warn : ns_conn method not implemented yet 
Warn : ns_conn url not fully implemented yet 

Warn : ns_conn query not implemented yet 

memoize is not working... I need to write a few more tests to make sure that it can interoperate with ns_cache okay.
also,  do I need to flush the cache on startup (this seems reasonable)


I got distracted testing the memoize thing.  I thought there was a bugin that sometimes db_foreach didn't work correctly.  (I need to point michael to the regression test.)


So give the memoize thing another chance and see if ns_cache eval is working correctly


ns_cache is messed up.  get doesn't really work if the value doesn't exist (it returns 0 not "").  Check the doc URL and try it again with 0or1row etc.

ns_cache is working okay.  Now the problem seems to be site_nodes_sync_helper/db_foreach.  Flush is not working.
Bizaare.

Rather than fix this,  see if I can rewrite the function to properly init the nsv array.  Is there something about the foreach statement of the query dispatcher mechanism that pins the database connection?

Having trouble with adp__parse .. .possible just the case when master src="" is not defined
  
&lt;verbatim&gt;
errmsg is can't read "code": no such variable 
&lt;/verbatim&gt;


registers
keyboard macro
repeat command
abbreviations


(fset 'foobard
   [?H ?e ?l ?l ?o ?  ?t ?h ?e ?r ?e return return return ?E ?t ?e ?n ?n ?e ?u ?s ?i ?e ?s return return])


 acs-integration-procs.tcl would be a good place to start`


I had thought intercepting all calls at the return.template level (in acs-integration-procs) would be the best approach,  but I believe adp.parse does some code caching that I'll need.


Approach 
If there's no ad.return.template at the end of tcl file,  add it
Otherwise
The chunk thing doesn't work really well.
It's assuming that OpenACS code is parsing the conditional tags,  whereas I just want to use nstcl.



Okay - I can't handle master templates... no big deal.  I wonder if I can handle nested includes ... arg!?


Things that are wrong:
index.adp is the file being served -&gt; should probably be index.tcl?
what is the difference between  fancy &amp; plain adp parsing?

ns.register.adptag is not doing anything... that's why master is not working.
How does ns.adp.parse interact with OpenACS codes?

What should ns.register.adptag be doing?

Investigate this a little further than put it aside and figure out bindings.

Doesn't work unless user is logged in.  Sigh.

ns.adp.parse... do I simply write a SAX parser and map it to the tag handlers?

Okay ... I really need to replace ns.adp.parse,  because that's the routine that's calling registered 


Problems:  ns.adp.parse does more than just parse,  it will look for an execute a tcl file,  and also perform the caching.
I don't have a good way to implement the ns.register.tag cmd.  This shouldn't matter.

Project:  can I get foo to work correctly?
How many adp files exist without tcl files?

Nevermind performance for now.  
Write a proc which will execute a template in the stack frame of the tcl file.
paste this into  return.template and parse.ad.conn.file

So I'll need to tweak the integration-procs and the 
&lt;verbatim&gt;ad.return.template&lt;/verbatim&gt;

integration procs&lt;verbatim&gt;adp_parse_ad_conn_file&lt;/verbatim&gt;

request-processor-procs
&lt;verbatim&gt;ad_proc -private rp_handle_adp_request {} {&lt;/verbatim&gt;

Hack the master tag to handle the default template.  (where would it find this file?)


I had changed index.tcl to call adp routines directly.

Remember that I have to comment out 1) parse-procs and 2) tags (not strictly necessary)

I'm going to try completely commenting out the /acs-templating/tcl directory

Success! Everything worked when I only executed template utilities and used the following code in go.tcl:

&lt;example&gt;
set ::nstcl::template::default_master c:/projects/portable.nsd/openacs-4/www/default-master

ad_user_login 2495

rp_filter "Because I said so"
source c:/projects/portable.nsd/openacs-4/www/index.tcl

puts [ ns_adp_parse -file "c:/projects/portable.nsd/openacs-4/www/index.adp"]
&lt;/example&gt;


NEXT STEPS:

Don't worry about handling default masters at the moment

Write a simple wrapper for tclhttpd.  How do I point to code that is just initialized?
How do I populate the URL? (should be easy)
How do I map each URL to my proc (should be easy)

11/21/2002
WOOHOO.  Milestone 1 reached!  tclhttpd can server openACS pages.

MAJOR BUGS:  1) X request-processor doesn't work as a proc
             2) X site nodes doesn't resolve /index.tcl references to a node (X)
             3) repeated 
             3) first page requires login
             4) not thread safe by any means (but I don't care)
             5) ns_schedule not implemented
             6) Xad_conn unimplemented

tclhttpd Release Notes :  you must comment out the admin stuff.

 cd c:/temp/tclhttpd/
 tclsh ./bin./httpd.tcl -debug 1

net use w: \\192.168.101.82\web /User:john


I'm going to try to fix 2) in the next 1.5 hr by comparing it to native openacs.

http://127.0.0.1:8015/


Friday : 
MAJOR BUGS: XCookies,  XHeaders
MINOR BUGS: 1st page req. login
REGRESS:   File Upload?

TCL Currying:
http://mini.net/tcl/2882 

Write something into working openacs installation to export headers.

Handle ns.write by writing into data and ignore/translate header stuff.

There was a bug in the xparser routine that replaced all instances of "" with a particular xql query.  Broken regex.  This was probably responsible for both cookies-not-working-correctly bug,  as well as user-must-be-logged-in bug.

Also,  xparser cannot convert queries to brackets if there are executable statements within it ( [] or $]}


MAJOR BUGS:  (X)why can't it find /admin ???

ad.page.contract not setting defaults.  This is also the first-page-req-login bug.  (Actually I think this is a nstcl error) 
 
MINOR :      add a buffer to the sock array for ns_write. xparser is messed up.
        make sure redirect works
        (X)master-template support
        incomplete ns_write support
        inline tcl in adp not working..?

Check out globals and cleanup between requests.  Is ad.page.contract doing it's job?


Shoot - I think this works except for the query extractor.  

Admin,  make some fixes to query extractor to get some pages working.  (edit user)

Fix two query parser bugs:  
   1) when either dollar sign or brackets are in query
   2)                          

Monday 
Fix the "" xparse.pl replacement bug  (FIXED?  but everything is not yet reparsed
Fix the problem with nested dollar signs or brackets.

Fix redirect error when double forward slashes exist //

Does rowcount exist in templates?  Seems to exist,  but it's not always defined?.
This issue relates to stack frame of template evaluation.  include files are not currently working.

X Implement :  ns_url2file 

Theory: What I've done wrt source'ing tcl files does not work with include'd templates,  bc they're not evaluated in the stackframe.


Do ad.page.contract properties matter?

I am stuck on the template thing.  Skip it and go back to the 

FastCGI Works!!!!

Here's the apache voodoo:  (thanks to http://members.optushome.com.au/projectgoanna/installation.htm )
&lt;example&gt;
Alias /fcgi/ "/Apache/fcgi/"
&lt;Directory /Apache/fcgi&gt;
	   SetHandler fastcgi-script 
	   Allow from all
	   Options ExecCGI
&lt;/Directory&gt;
&lt;/example&gt;

FastCgiExternalServer /Apache/fcgi/openacs.tcls -host localhost:8999

[c:/apache]      ./apache -f conf/httpd.default.conf 
[c:/apache/fcgi/]tclsh ./openacs.tcls -port 8999
or
[c:/apache/fcgi/]tclsh ./foo.tcls -port 8999
or
[c:/apache/fcgi/]tclsh ./pnsd-fastcgi.tcl -port 8999

[note that name in FastCgiExternalServer doesn't matter] It just has to be consistent with the Rewrite directive.


http://localhost/fcgi/openacs.tcls

It will take a bit of work to get something as complicated as openacs to run under  fcgi


Tuesday TODO:
Get home page working again in go.tcl.
Get home page working in fcgi.tcl.
Get rid of debugging code in templating procs.  Return it to normal.

I had a bug due to the redefinition of template::multirow in query-procs.tcl.  I commented this out and we're back to working.  I must say it's pretty cool to have tclhttpd/apache+mod_fcgi/tclsh debugging options.

I have to figure out how to redirect log:: to a file.  I think this will require overloading ::log::Puts.... nah.  I can use the  ::log::lvChannelForall [open $logfile]

FastCGI todo;
mod_rewrite
cookies
investigate dynamic FastCGI server voodoo
other ns_ things ...


At this point,  with little bits of time,  set up mod_rewrite to redirect all requests to my fastcgi instance

I need to redirect my efforts to solving the templating implementation.  I need to address the stack frame bug by writing a prototype of multirow data sources used by nested templates,  and hack MC's code so I can see what code is being generated and when.

I should also start working on documentation etc.


I can generate the code for template parsing... now I should paste it into teh tcl file directly in place of ad_return_template and see if it works.


Aha!  What OpenACS does is create a procedure for the tcl code being evaluated (using some mime-type convention),  then it uplevels the ns_adp_eval into that procedure.  I need to find out where and how this is done.

Alternatively,  just cat the page source into the compiled proc.  That should work well.  (Err... no.  The same template should be able to be driven by different data sources... I shouldn't comingle them.

adp_init grabs the tcl file source and stores it in a procedure if it has changed.  

adp_parse -&gt; execute proc returned by init.  Then 


What I'm doing in the integration procs should really be moved to adp_parse


Monday 12/3
Stopping point reached.  

Migrate stuff that shouldn't be in acs-integration into adp_parse

Test without going through entire rp_handler
Start over with example in foo.tcl,  move to go.tcl,  and slowly migrate into procs

chad 617 480-9129  (cell)

Figure out what happens when info body is called in the foo.tcl example that works.
Why are stack things way out of whack?


Tuesday

EUREKA!  This is all about the present directory ... the template looks for include files in the current directory, instead of in the directory the template is located in.  Quick fix,

MAJOR 
Let Michael know about fixes to template invocation.
MINOR 


NsTclToDo 


XWhat I would like to do is make a rewrite rule to send all requests to the fastcgi handler.  But how do I maintain the arguments and parse off uri from the rest?  Done.

Rewrite reference:
http://www.zope.org/Members/kedai/apache_zope_fcgi

Then make sure cookies work?

X Get the logging code working.


Now I've got to debug the sending of regular files... implementing :
I think I'll just read the file and then stream it to stdout.  Probably grabbing code from tclhttpd?  
Optimization TODO: I should really make sure caching headers are done correctly,  and that apache can cache these so I'm not spinning my TCL wheels doing static web serving.  Opt#2 Can I prepopulate the cache?

12.09.2002

TMW: Fastcgi working to serve pages.  
- 404 error is X- Make sure template works without login (acs_properties)
- I've got to get the header worked out so cookies get functional.
- I need to be able to replace the cookies... so I should probably use an ad_conn ns_set 
(LOW)Work out serving images by writing directly to FCGI socket?
- Images not working.


1) What should I do for the properties thing?

Figure out why login doesn't work? 
Do cookies work?
404 etc. 


*** 01.11.2003 13:29 PM
----

12.20.2002 15:46
I reverted to pg 7.2,  because of the source code !@$# in 7.3

I'm getting the error:
'invalid command name "gets"
    while executing' fcgi.tcl

I hit this bug before.  Why isn't it working?

The gets etc. commands are renamed to something else way up in fcgi, and then the namespace import command happens way later.  they should be one after the other.


12.21.2002 12:42

Let's see if I can get hello world to work.

I'll give foo.tcl a try - this just returns hello world.

It died as well.  I should revert Fcgi.tcl to it's original version

[Sat Dec 21 13:15:11 2002] [error] [client 127.0.0.1] FastCGI: failed to connect to server "/apache/fcgi/openacs.tcls": connect() failed



Try this with Tcl 8.4 (maybe it was a bug)
Try this on Unix  (shouldn't take long to test)



The solution was to put the renames directly above the namespace import commands.

Now it's working again.

Next up,  redirect.



Now the problem is 
[Sun Dec 22 12:08:13 2002] [error] [client 127.0.0.1] FastCGI: comm with server "/apache/fcgi/openacs.tcls" aborted: read failed
[Sun Dec 22 12:10:09 2002] [error] [client 127.0.0.1] FastCGI: failed to connect to server "/apache/fcgi/openacs.tcls": connect() failed

I can't get the fastcgi listener to work without crashing.  This is killing me.
I'mm going to make sure the redirect code works okay,


I'm working out redirect details,  and implementing namespaces

12.22.2002 18:08

Taking notes trying to get everything going on a rh 8 workstation.

Installed postgres.
hacked pg_hba.conf to allow local connections
createlang pgplsql template1
used the /sbin/service to restart
createdb openacs
createuser john, root etc.

Installed ActiveTCL (came with TclX)
ln -s /usr/bin/tcl to activetcl

installed FCgi -&gt; had to comment out line in makefile installing docs

Installed nstcl -&gt; need to use hacked sources

instal. tDOM
install CGI

There was a weird problem with [db_type].. I hardcoded it to be postgresql. (I need to look at this.

Aha - source code incompatibility .  In 20-db-bootstrap-procs.tcl I need to add the line "Postgres" to nsv_set ad_known_database_types.  (hmm - no this didn't seem to work.  What I really need to do is diff my nstcl with the cvs version.


After installation,  I need apache 1.3x 


12.22.2002 22:24
BACK IN ACTION!
Since namespace import was croaking


12.23.2002 00:22
Form argument parsing isn't working.  This is going to be simple when I pick it back up tmw.

12.23.2002 08:31
Okay,  I had to move the Fcgi library into my script, because the package require stuff wasn't happy with redefined gets and puts.  I moved package require TclX to the top

Running from the command line.
&lt;example&gt;
export SCRIPT_URL="/doc"
export REQUEST_URI="/doc"
&lt;/example&gt;


12.23.2002 11:54

I have to call cgi_input to have it converted from the headers (this reads in files too)

Now I'm hung up on cookies:  the headers are't getting set in the incoming headers variable.  I should probably do a copy or something.  instead of truncating the variable each time.

12.27.2002 07:52

Dump headers to screen to debug them.

Then add inline tcl parsing.


I think that the definition of the adp is getting overwritten.
I've got redirects working correctly,  but not cookie setting.  I think the order of headers is messed up.


This is getting all messed up because it's not resetting state.  Once the system goes down or hits a bug,  the headers etc. array is not unset.  

This might be only a problem with overwriting compiled adp page definitions.

&lt;example&gt;
$ pg_ctl -D /usr/local/pgsql/data/ status
pg_ctl: postmaster is running (pid: 1472)
Command line was:
/usr/bin/postmaster '-i' '-D' '/usr/local/pgsql/data'
&lt;/example&gt;


12.27.2002 11:43

What's not working,  besides openacs source code?

&lt;example&gt;
	ad_url "/acs-admin/users/one" maps to file "c:/projects/portable.nsd/openacs-4/packages/acs-admin/www/users/one.adp"
errmsg is can't read "all_group_membership(rownum)": no such element in array}&lt;/example&gt;



12.28.2002 13:20

I'm going to tackle some of the xql stuff,  just to get pages loaded.  It should make the other debugging a bit easier.

Nah - I'll keep focusing on the templating issues:
1) page_contract blowups.
2) reuse or overwriting of the same template


The second time through,  the array already exists on api-doc.

12.28.2002 15:39

This duplicate page  thing has something to do with doc_body_append,  I think.

Nope.  I think it's the disconnect between template::set_file and my own template glue code procs.

ad_return_template is returning nothing.

12.28.2002 16:10

Problem:  I must trace when things break and start returning the same page.  

group-types is broken - No pg analog.

12.28.2002 16:50

Same page errror was caused by not clearing out doc_properties array.  I should just delete the whole array, so parts are not shared from page to page.

So most of the errors now are due to strangeness in the xparser and the fact that &lt;% %&gt; things aren't done.

Aside: I keep waiting to hit the transition point where the big errors are all taken care of,  and it's just annoying small things left to crunch.

There is a problem with input parsing...  ah - it's a problem with parsing multiple parameters.   I bet it has something to do with my cgi-&gt;ns_set conversion.

Fixed this.

12.28.2002 17:38

The acs-workflow homepage works!  This is way cool.

Having trouble reading public_p.

NB:  I can determine how I was launched (to switch between fcgi/tclhttpd/tclsh/etc. by checking the [info nameofexecutable ] value.

How far do I want to get?
It would be terrific to get the xparser to work with 4.6.


&lt;if @error_msg@ ne ""&gt; must be changed to &lt;if @error_msg@ not nil&gt;



12.28.2002 18:20

API-Browser is working okay.
I would love to get workflow working...

But I'll see if admin pages are within reach.


In the templating code,  
The string compare stuff does not work with numbers.
And rownum is not getting set for one of the multirow

I'll need some type of hack for the string compare.

I have a fix for MC.


12.28.2002 18:54
explicitly setting bind vars not supported

[ad_table not supported]


12.28.2002 23:17

The big item on the agenda is defining a release.  I'd like to get enough working that people can get in and play,  but not have it be perfect.

To do that,  I really need to get down and implement the proc redefining stuff,  and make sure the xquery parser is working okay.

12.28.2002 23:48
Time to call it quits.  Error of the moment:
Improperly nested tags....

on /group-types/one.adp


12.29.2002 23:56
I cleaned up a couple of things in tclhttpd,  but there's still some weirdness.

I need to retrench and load up the latest source code tree from CVS


01.07.2003 09:08

I would like to see this work with Dynamic Servers,  so that I don't have to do quite as much setup.

1) Use chron to run scheduled jobs


__RELEASE DEFINED__ 


Get lars-blogger(?) or bboard(?) to work okay.
proto-type multiple interpreter backends to see what works/doesn't work
polish up xparser a bit more

After extract-bracketed works it's magic

[c:/apache]./apache -f conf/httpd.fcgi.conf 
[c:/projects/portable-nsd/]tclsh pnsd-fastcgi.tcl -port 8999

01.07.2003 11:19

Fix logging.
Document how to patch an existing openacs source tree.
Rename proc so that functions and overlapping files are not loaded.

Reinstall software.  Include bboard.  See if I can get basic, non-scheduled functionality working okay.


01.07.2003 12:13

Okay.  This doesn't do anything interesting.  I should have a go at the bboard,  then work on upgrading to TCL 8.4, then OpenACS 4.6

This will shake out bugs in the xparser,  and allow a greater pool of people to play with it.

Can I get this to cluster correctly on Windows?  That means production Windows usage is a possibility.

I'll write a document that demonstrates how to set it up (patch source code etc.)  and then just post the code.


I just figured out what the next step should be.  Implementing query dispatcher compatibility in nstcl does two very important things:
1) fixes a lot of niggling bugs in the existing
2) I can just ship my compatibility layer
3) The 4.6 port and new module support will be greatly simplified.

At that point,  I could begin the writeup and release the project and try to enlist some interested parties.

Simultaneous with this I'll try to figure out how to coexist with existing source code.

What calls do I need to intercept and how is it done in OpenACS


set full_statement_name [db_qd_get_fullname $statement_name]
    set sql [db_qd_replace_sql $statement_name $pre_sql]
db_qd_log
db_qd_load_query_file


file://c:/temp/openacs-4/packages/acs-bootstrap-installer/tcl/40-db-query-dispatcher-procs.tcl 


* Source Code modifications to stock OpenACS installation:

Two big areas:  templating  and database calls
code ad_proc to ignore cert

skip if source code path ends in {}
or function name in {}

intercept proc_doc, ad_proc
rename ad_proc ad_proc_orig
proc ad_proc { args }
   ad_parse 

db_string
db_list
db_list_of_lists
db_list_of_ns_sets
db_multirow
db_0or1row
db_transaction
db_abort_transaction
db_name
db_dml
db_exec
db_exec_plpgsql
db_exec_plsql
db_foreach
db_abort_transaction_p

template::multirow
ad_return_template
adp_parse_ad_conn_file
template::request::error
template::widget::file
template::request

The other way to do this is warn if we're redefining a command.
Then I'll have a list of things that the openacs source code redefines.  
I can either dynamically use that list,  or feed it into an array and be explicit.

Full source code support is the way to go.
Tclx has a neat-o debugger.

&lt;example&gt;
#package require Tclx
#cmdtrace on notruncate
&lt;/example&gt;

is awesome.

01.08.2003 09:08

Having trouble parsing the query files because of a -persist flag.  I remember this. It's another proc I had to overload.

Solution -&gt; hack "ns_xml parse" to look for and ignore -persist flag.

01.08.2003 10:20

Now some of the query files are not being parsed.  I have a feeling that this is due to the lack of CDATA references.

I think the query dispatcher piece just needs to know which source file it is being called from.  Then it should work okay.

01.08.2003 16:23

This breaks nstcl...
&lt;example&gt;
    select token_id, token
    from secret_tokens,
         (select trunc(random()*(select count(*)-15 from secret_tokens))::integer as first) r
    where token_id &gt;= r.first and r.first+15 &gt; token_id;
&lt;/example&gt;
...typecast still seems broken


01.08.2003 17:19

I think I have the database part of openacs taken care of.  Now it's time to address the template part of it.

A-ha.  We have a problem,  because database calls made from within a template tcl file will get wrapped up in a proc... and will lose the information of the query fullname.  Grrr.

Quick hack:  Resolve the fullname at template compile time and store it in the template.  I'll have to have the proc overloader look for the special fullname array in the proc namespace,  then proceed as default.


multirow requires special handling because it has more args than normal.
finish initial multirow implementation.



Tomorrow: fix adam's stuff.

put some writelines into the fullname resolver.


01.09.2003 12:07

I'm having a  bug I had yesterday where the &gt; is turned into an entity reference before being fed into the query dispatcher.  How do I fix this on the laptop?

**Starting from openacs distro.
I *think* you need to change the one security query that uses ::integer to a cast.

The other issue is the master tag without a src=

01.09.2003 13:22

I'm sick of QD parsing.  I'm going to serialize it to a file using 

http://mini.net/tcl/3469  

And then overload the QD code so that it doesn't load everything in.

This should speed things up alot.

It did.

WANT it to look for: dbqd.acs-subsite.www.index.site_nodes
Looking for: dbqd.www.index.site_nodes


Actually I was mistaken.  the [root]/www/index-postgres.xql file contains a query that should be indexed as dbqd.www.index.site_nodes,  but somehow it's not getting read.

I fixed that problem by hard-coding the load of the qd file,  but I need to make this automatic from the ns_ or ad_parse_adp? routines.

01.09.2003 14:55

I'm trying to construct an xql.dat file that contains the query I need,  so I can debug the template code.

I'll need to revisit the adp/xql loading mechanism.

Why does everything die?

01.09.2003 18:09

I must stop.  The issue is that the page contract variables are being set in a different stack frame.  I wish this was an easiere problem to debug.  I could be that one of the routines I've defined has been redefined .... maybe.

01.10.2003 09:50

Okay - I've figured out two of the problems.  tclpro is dying because pool is being redefined by htmlparse after it has already set up pools.  I'd like to make it robust against that happening.

Problem 2) the qd is mistakenly figuring out the query fullname, because the adp files are being called in a different context than normally.

This line in '40-qd-procs'...
    set list_of_source_procs {ns_sourceproc apm_source template::adp_parse template::frm_page_handler rp_handle_tcl_request}
determines whether the qd knows to resolve fullname as a URL.


01.10.2003 14:54
Why is default master and master templates a problem now?  Because fixes I've previously made have been removed.
I need to produce a diff of my tree vs. the stock install - that'd probably save some time.


Are the template tag routines from the stock installation confusing me?
Is template_tag master screwing things up??


I'm packaging up the code for Andrew P. I needed a lot of files from the root,  + the tclhttpd file,  plus my version of nstcl.

Contact Chip/Jack:

http://openacs.org/forums/message-view?message_id=50455 

http://127.0.0.1:10080/S/Z/Zoe.E/SZ/1B837300DB80B779A600F200875F7DAE/1.0.5.1.3.1.1.0.7.1.0.1



Freewrap
http://freewrap.sourceforge.net/ 
=======
01.15.2003 23:42


01.28.2003 12:35

1) Does query dispatcher really work?

2 )Come up with list of outstanding issues.
Make sure Query Dispatcher is working okay.
LEO-ize documentation.
Investigate IIS support w/error page  

Did I get query dispatcher code working on one machine and not the other?
Should I run the FastCGI tclsh/Pg combo on my desktop for speed?

I absolutely must get a tags file created.

Things die after a single error.  Not good.

I should start creating an automated test for these errors.

It seems as though once things are cached, they no longer work.

How does nstcl handle using arrays.  [ set $varname ] doesn't work anymore.  This is probably my bug.



Fix the array bug-&gt; that seems easy.  Try to determine if xql.dat contains all of the adp/tcl pairs.
Migrate tcl part of this over to cbr900rr


=======

*** 01.28.2003 23:51 PM
----


The problem with the installer (and my implementation of the QD in general)  is related to the postgres-specific database proc's.  I haven't done any overloading of these,  and consequently they're kinda messed up.

Clean these up and get the installer working(!).  This should fix some other bugs as well.



*** 02.01.2003 12:59 PM
----



perl d:/Projects/portable.nsd/bin/acstags.pl


I surrender.  I can't get the query dispatcher to come up with the right name for auto-install queries,  so I'm just going to paste in the right ones.


Adding a tag to templating is turning out to be a bitch.


*** 02.03.2003 13:49 PM
----


Okay,  I'm very close to implementing the &lt;% tags in adp pages.
I need to do a find/replace on tags,  and catch instances where tags contain an equals sign.

&lt;%= %&gt;  will transform to append &lt;% adp_puts %&gt;
&lt;% adp_puts will transform to uplevel 1 {


Now do a find/replace on &lt;%\w+= -&gt; &lt;tcl&gt; adp_puts; &lt;% &lt;tcl&gt; %&gt; &lt;/tcl&gt;


I need to hide the code from nstcl's aggressive hide/replace.  I should go through a single pass of the code, collect snippets into 
array of procs.  Re-expand code snippets on parsing.



*** 02.04.2003 08:20 AM
----

My portable.nsd initiative seems a bit misguided, given how much of the AOLServer API is being debundled from AOLServer.  
I guess there's still a need to figure out how to replace the web server component (Apache/IIS/tclsh bindings) 


*** 02.04.2003 10:29 AM
----

Make loading XQL files work,  and cache it.
Make adp work ... QD must know I'm running under templating lib.


I had collapse adp_parse and adp_parse_ad_conn_file into adp_parse_ad_conn_file,  but that made it very difficult to get the QD working correctly.

I broke them out again,  and tweaked adp_parse just slightly to be nstcl compliant.
Now I don't have to overload adp_parse_ad_conn_file at all (!). 

*** 02.04.2003 18:22 PM
----


The auto installer breaks because the code running doesn't match the xql file? .. i.e. do I need to hard-code the URL to /auto-install

Fixed it.  I wasn't loading the zz file.  Now I do a find on that directory, a sort, then I map source to it.
 




*** 02.04.2003 20:26 PM
----

There is a subtle bug in the embedded TCL code.  Figure that out,  move on to ad_table.

The code is being executed twice !



*** 02.05.2003 17:00 PM
----


There is a bug when the host differs from the web server.
Debugging the code on a different machine than the web server is way cool.

*** 02.06.2003 11:24 AM
----


Interesting host name issues when I started running the TCL on  another server.  I think I've resolved them.

I have also a caching problem with the ADP files.  I believe it's returning the mtime instead of running the query.

*** 02.06.2003 18:17 PM
----

Templating is not really working.  I think it's because the variables are still around after the thing is executed.


I don't have a good debugging strategy yet.  I'm not even sure what the problem is.  It looks like it works the first time through,  but the second time it calls the master template in a different way.  It has something to do with variables being defined in the global stack frame.

The adp_init_properties should work out okay... because it shouldn't try to redefine anything that already exists.  The problem is that it's getting called in the global frame,  and I guess 'variable' or info exists behaves differently.


db_multirow is broken... not QD'ing.

*** 02.07.2003 11:35 AM
----


Okay.

I need to put tweaking the bits aside.

Improve installation docs and 
Get installer with no packages working okay.
Get FastCGI working with Named pipes.


They both seem to be writing output correctly.  Oddly, the trace file I create using cmdtrace gets the output,  but it doesn't go to the pipe that is created.


02.09.2003 15:35
Things that could be wrong:
 tclx is mapping stdin to somewhere weird... can I write to the socket directly? -&gt; but doing puts?
 -&gt; get rid of CGI library.
 

I've located the problem.  server_accept is no longer a tclx command,  so that part is broken.  Since the rest of the fcgi library works using regular sockets,  I should be in good shape witht them.

The next step is to work out the release notes and announcement.

Try to get the c-src one to run


02.09.2003 21:33



02.25.2003 10:02

Okay- I'm using a tclIndex file and autopath to load openacs functionality on the fly.  This should hasten testing, and make COM object and CGI deployment a reality.  It'll be very important to write tests for these implementations so that I can see what's broken.

02.25.2003 12:57

What is going on with the database handles?  I suspect that the query dispatcher is kicking off a database routine,  and making sure hte whole process goes into a loop.

working... not sure what I did. When I source library routines,  I must use apm_source so the query dispatcher doesn't freak.  This bug will come back to haunt me when autoloading

So I should change my tclIndex function accordingly

What is slow:  
1) secure tokens
2) query dispatcher? -&gt; move this to cached/memoized
3) page compilation?

My code is out of sync with cbr900rr ... I need the template file to mmake this work

Move to installer.  Huh... installer works okay.  Nice.
Next up would be a sample unit test... I would like to put together a framework that catches error messages when they're logged.

TODO: 
  * How do I more quickly handle security,  given that I'm running outside a web browser... should I just hard_code ad_conn user_id to return a overridden value?
  * change tclIndex code to use apm_source instead of source (for which files?)
  * the whole idea here was to enable cgi/com ... what needs to be streamlined to make that a reality (mostly security) memoizing
  * Cache TCL compiled procs from template::.
  
  * What does memoize do exactly?


02.26.2003 00:26

{can't read "node(url)": no such element in array}

02.28.2003 09:46

How do I fool the dispatcher to always return the correct path?


02.28.2003 10:02

Ideas for making query dispatcher more robust:

Create a command associated with an XQL file that takes the key and returns the query based on DB type.  
Using currying to jam this command name into each proc?  

When file is being parsed and commands defined,  write in the value of the file in which the proc is defined.
I would have to overload both ad_proc and proc to do this.  How would this work for templating/web pages defined by nstcl-templating/adp_compile ?





03.11.2003 07:32
http://www.oche.de/~akupries/soft/ptools/  


I need to release a new version of the software.

03.12.2003 08:18

I'm going to implement ad_table.  Then revisit the query dispatcher bugs in the auto-loader.


This is going whole hog,  but I should create a multirow table source,  append the addition records to it,  then iterate over that.
This way I'll separate out the ugly date munging code from the presentation code.

Having trouble with this query... I'd like to make a union view to have.

Okay,  much simpler approach.
replace db_with_handle Tdb with 
    set full_statement_name [db_qd_get_fullname $statement_name]
    if [catch {set sql [ db_qd_replace_sql $full_statement_name $sql_qry]} ] {
	set sql $sql_qry
    }
    # This procedure needs a full rewrite!
    #    db_with_handle Tdb {}
	# Execute the query
    set Tdb [ns_db gethandle]
    set selection [ns_db select $Tdb $sql]

Now,  I'm having trouble once again with the query dispatcher.  I don't know where it thinks we're running,  but it's pulling the wrong query for the following function:

::nstcl::db_string apm_package_version_installed_p {


03.12.2003 12:19

I've got some of ad_table working,  but you can't call queries within ad_table.  If ComplySite doesn't need this,  I won't do it.

I should write a replacement for the request processor so that it only looks in the location ... er.  Actually the easiest way to do this is to bypass the request processor.

Easiest approach: strip out all openacs code
 
Mount the www folder at the root of the CGI site.  
Disable memoize.  Hardcode site_nodes.
Hardcode the package ID in ad_conn.  
Cache the XQL files if they don't exist.  
Don't precompute security tokens... get them on demand.

What's the best way to preinitialize the CGI code? Can I set an environmental variable (TCLLIBPATH) to tweak the autoload path to point to an pnsd-created index?


Next steps: test the autoload code once more to see if it works.
Once this is stateless,  see if I can get mod_tcl to work with apache 2.0.



03.12.2003 23:41

Installing mod_tcl:
First tried apt-get install tcl.  This installed 8.3.  apt-get remove tcl8.3
Then d/l'd activestate 8.4.2. ran install.sh
Then ln -s /usr/local/ActiveTcl842/bin/tclsh /usr/bin/tclsh
Then... to get man pages working:
debian:~/ActiveTcl8.4.2.0-linux-ix86# export PATH="/usr/local/ActiveTcl842/bin:$PATH"

Now I'm downloading tcl sources from www.tcl.tk.  configure/make/make test/make install

Try this with redhat.  See if I can install a separate version of A2 in a different directory.
Or just start over with a naked redhat instance.

I changed the /etc/apt/sources.list to unstable,  then ran apt-get update

http://www.ibooklinux.net/php/ibooklinux_g_h.php3?single=8 


03.14.2003 08:28

Here are my ideas for supporting ComplySite
  -use mod_env to set the autopath for cgi variables
  -what can I run to auto-load code before ?
  -then just run as CGI.
  -hack rp_filter to set up dummy ad_conn... or does ns_misc/etc already do this?

Load Balancing
http://siag.nu/pen/ 


mod_backhand
balance.sf.net


PROBLEM:  qd is using  decode(count(*),1,0,0) to figure out whether a table exxists... this is the Oracle version.
Either the QD table is not getting loaded,  or 




03.18.2003 07:15
It seems as though rivet is the way to go with Apache.


TODO: create a script to create the library tclIndex file

I'm getting bogged down in this... It would have been great to define a set of tests to catch things.

TODO: Work on  xql creator so it loads files it hasn't seen yet.  Move it to the database.
Make test to check whether I've broken templating.  Very important.


Code that works on the laptop does not work on the workstation.   I should see if I can fix it,  otherwise just use the laptop for now and write a test to reproduce it.

Broken:  I need to make sure the mutex junk is handled correctly,  also to run the after 

Okay ... I've determined that not apm_source'ing acs-templating breaks my code.  Find out what proc's are needed,  then make sure they're not be redefined.


It's working.  I needed to call apm_bootstrap_load_libraries -init  acs-templating  to get this to work.


Next step... 
1)generate a test for this bootstrap code loading a URL.
Fix login bug.  
2)Do some profiling/generate security cookies on the fly  to get subsecond response time.
Then,  write installer for complysite.  mount it in Apache, set response handler

Get this working with Oracle proc's ...rewrite a few cases


03.19.2003 10:02

Work on test a little bit,  then address things that can be cached.  Site map can be cached to a tcl file
I should realy

X1) generate a test for this bootstrap code loading a URL.
Fix login bug.  
2)Do some profiling/generate security cookies on the fly  to get subsecond response time.
Then,  write installer for complysite.  mount it in Apache, set response handler

Get this working with Oracle proc's

Can I make connection-opening tranparent?  Or on demand somehow?


03.19.2003 11:26

Arg.  url.test  was working a second ago.

Skip db-init-checks
Okay... This was broken because at some point the code performs a sync which blows all nodes away.  I think.
Because I died in the middle of a load,  one of the database initialization functions misbehaved.


EUREKA! This got me the profiling information I needed.

package require Tclx
profile on
source [file join [file dirname [info script]] pnsd-bootstrap.tcl ]
profile off x
profrep x real


1) First order of business is to cache templates.  Then see if I have a workable system using the cgi.tcl code.
2) I would also like to cache the site-map into a source-able tcl file.
$root/cache/data
$root/cache/tmpl/xxx.tclc

03.19.2003 14:12

puts [time { 
    source [file join [file dirname [info script]] pnsd-bootstrap.tcl ]

    set ::pnsd::url /
    set ::pnsd::querystring ""

#ad_user_login 2537

    rp_filter ""
    rp_handler
} ]
Yields:
4172000 microseconds per iteration



03.19.2003 14:30

What is this?
error     {nsv_get failed for ad_page_contract_mutex ( filters ) }



GRAND PLAN: 
cache all the code needed to run a page in the compiled template.  
Add as few 'source's as necessary to bring in data and do initialization.
Use apache-rewrite

I've decided getting it faster than 3-4 seconds will be too error prone and destructive.  I'm best off combining mod_rewrite and a fastcgi based solution so that I can start working on making it a practical system (something people can use).




03.19.2003 15:43

Now the plan is launch separate interpreters from Tcl,  run them under supervise.  Have apache round robin between interpreters.  It would be great to have some way to reset interpreters... to expire them.  But I need to push the ball further.



[c:/apache]      
./apache -f conf/httpd.fcgi.conf 

[c:/apache/fcgi/]tclsh ./openacs.tcls -port 8999
or
[c:/apache/fcgi/]tclsh ./foo.tcls -port 8999
or
[c:/projects/portable-nsd/]tclsh ./pnsd-fast.tcls -port 8999

03.20.2003 09:58


TODO: Write installer for Complysite.  
=======
Form data not being accepted.  so password submit not working...
Ah-ha... I think this is a memoize problem


Weird that I didn't get this bug before.


Use URL Rewriting guide to 

03.22.2003 10:27

Could the problem be 

[write the big list of persistent arrays to the database... dump/load this big file.]

X Scripts I need to write:  xql assembler ... source pnsd-init.tcl; load_xql;

exec_pgplsql is broken when you don't use apm_source to execute your sp's because the QD gets confused.

I'm shutting down. When getting a query dispatcher error,  check to make sure I'm accessing the code via one of the approved methods (apm_source being the one I know about for init files,  and auto-installer).

On to working on FastCGI.

Get traintrak installer working... can I load tt pages?  Write some tt unit tests... perhaps this is a different project/folder?

Code organization: heed Will Duquette's packaging ideas??  Move all procs into pnsd namespace.  Then import them.

IDEA: Experiment with supplying either package name or actual query text via modified proc definition.  Then use a closure-like mechanism for adding query to proc.


03.28.2003 19:17

X I'm just going to tackle some annoying bugs.  The login issue.  Then perhaps the package mounting error?

X The query dispatcher is just plain broken for normal web pages.  This is bad.  Should I try radical surgery,  or just to fix it for now?  The latter.

A-HA- the broken pages resulted from items not being in the cache.  So - the solution is to write a little script to create the xql.dat

X Fix QD.  Why is name broken?

Need to debug this.  it'd be nifty to write a test

Hrm. It looks like the init proc is called twice. -- It is,  the second time for the master template

Did I make a hack to the init_properties routine? No


03.28.2003 21:58

Okay... Login is broken for "/" when running using go.tcl.  This was working previously.  And in fact it works okay via fastCGI.  

03.29.2003 06:24
Clearly the init'ing code is doing less than nothing --&gt; it's overwriting the name property with form_vars.
What do I get when I disable it?

A-HA.  Damn collision of variables because of all the uplevels.  This is bad coding design on my part... I'll have to evolve away from it.


Here's the big error:

bind variable "" does not exist

The problem is definitely in the db_exec_plpgsql function,  and the way it handles bind var's.  That's where the installer keeps crashing.  I need to get over this hurdle so that I can complete the traintrak installer.

There was an error in one of the apm...xql files...

		    node_id := site_node__new(
                             null ***MISSING COMMA***
			     main_site_id,


The problem is in nstcl not knowing how to come up with a return value.

Okay - the unsubstituted version of the instantiate SQL is being passed to bind-var.  I think this is because the query dispatcher is being called twice.  I need to turn it off for this proc.

That was the problem... the QD was getting called twice,  and the pgplsql substutition was getting rolled back.

REMEMBER THAT I HAD TO HACK NSTCL ns_pg_bind and OPENACS (xql)

How can I modify catch to create a proc and then execute it,  so that I can have a trace???

03.29.2003 12:53

The traintrak stuff should load just fine (confirm this.)   Run through the code writing tests etc.




=======

In ... d:/temp/openacs-4/packages/glossary/tcl/glossary-init.tcl change ns_share line to ...
set ad_glossary_filters_installed 0

(Though it'd be cool to fix ns_share.

TODO:  Fix logged in bug?
Bind vars support missing for ad_table ...

Redirect broken.  Sometimes. (this is probably a stray 'puts'


03.20.2003 14:52

Load Complysite application.  Mount it,  run it.


I would like to programmatically install the complysite application.  


03.20.2003 17:08

Quickest way to get Complysite up and running.  Install data model file directly.
Install and mount package at root URL.

replace subsite with complysite instance.


I have to mount the package somewhere


I am close--- oh so very close. 

I need to mount the package... then take a look at the code.
I should send a bug report to Chip.


The installer has worked in the past.  This bind variable is lame.

Write a test that reproduces the problem

03.20.2003 21:06

Okay,  this is kind of messed up.  I would like to create a VM,  then reset it after each test,  or run some sort of pg dump/load script.

Tomorrow,  re-run installation script.  Still need to fix bug in mount application.



Start using nstcl::ns_config


It seems pgplsql is broken.  That's actually not terrible, because I should be able to address all my installer issues if I fix it.

This went away when I went to processing xql files.


03.21.2003 11:54

Doing an install,  I keep timing out at 117506, right after the search engine is installed.  I wonder if it tries to exec an OS command line?

Grr.

log.txt at line 116228 @  03.21.2003 13:14

Idea for compiler:  
Load up interp with all openacs commands
Look for all commands in a particular (cgi)file...  get the [info body]source for them... repeat on the sourced files.  This defines the header.
Streamline initialization code.

You can compile ahead of time... then bytecode compile using prowrap.



* TO CREATE OACS_FULLQUERIES:

set LOAD_XQL_FROM_CACHE 0 ; #make sure to do this in init file

source pnsd-init.tcl;  ::pnsd::source_openacs
pnsd::load_xql

This doesn't work... WHY?



X Fix problem loading index page... it has to do with the __name variable that has vexed me.
this is fixed by using upvar, instead of bad uplevel.

I need to regenerate the OACS_FULLQUERIES when I mount a new package. Sigh.

</t>
<t tx="T5"></t>
<t tx="T6"># Copyright 2003   -   John Sequeira  -   http://www.pobox.com/~johnseq
# johnseq@pobox.com
</t>
<t tx="T7"></t>
<t tx="T8"># pnsd-fastcgi.tcl - OpenACS/FastCGI commands
# 	$Id: pnsd.leo,v 1.4 2004/01/21 08:02:18 john Exp $	
#
# This script loads the OpenACS commands and packages into a tcl interpreter instance,  and sets up a wait
# loop to respond to FastCGI client requests.  
#  
# It can be used to serve OpenACS pages from any FastCGI enabled server (Apache, IIS, Zeus, Tux, etc)
# 
# TODO: 
# -The memoize command is vital to getting decent performance from OpenACS,  and it has not been fully implemented.  
# -Scheduled procs are no-ops.  I need to figure out how to launch an interpreter who's sole job is to run scheduled procs.
#   This will likely be platform dependent (a service(?) on windows, a cron job elsewhere, an Apache startup directive, etc.) 
# - Only currently working on Windows as FastCGIExternalServer.  This will require either manual set-up of multi-process launcher and round-robing functionality,  or use of a threaded interpreter instance (which we'd like to avoid)
#
# NOTE:  It includes an inline, slightly modified version of Tom Poindexter's Fcgi.tcl that 
#  fixes a strange bug regarding package loading and redefining the 'gets' and 'reads' command.
#
# 
# JS - 11/2002
#
&lt;&lt;copyright&gt;&gt;


###############################################################################
#
# fcgi.tcl
#
# Copyright 1998, Tom Poindexter,  all rights reserved
# tpoindex@nyx.net
# http://www.nyx.net/~tpoindex
#  see the file LICENSE.TERMS for complete copyright and licensing info
#
#  FastCGI interface for Tcl 8.0
#    Extended Tcl (aka Tclx) is required for 'AppClass' style connections,
#    (the 'package require Tclx' is near the bottom of this file, and only
#    used when needed.)
#    Tcl 8.0+ is required, as fcgi.tcl uses namespace and binary commands.
#    

namespace eval fcgi {
 
variable fcgi
global env
 
###############################################################################
# define fcgi constants (from fastcgi.h)
 
# Values for protocol info
set fcgi(FCGI_LISTENSOCK_FILENO) 0
set fcgi(FCGI_MAX_LENGTH)        [expr 0xffff]
set fcgi(FCGI_HEADER_LEN)        8
set fcgi(FCGI_VERSION_1)         1
 
# Values for type component of FCGI_Header
set fcgi(FCGI_BEGIN_REQUEST)       1
set fcgi(FCGI_ABORT_REQUEST)       2
set fcgi(FCGI_END_REQUEST)         3
set fcgi(FCGI_PARAMS)              4
set fcgi(FCGI_STDIN)               5
set fcgi(FCGI_STDOUT)              6
set fcgi(FCGI_STDERR)              7
set fcgi(FCGI_DATA)                8
set fcgi(FCGI_GET_VALUES)          9
set fcgi(FCGI_GET_VALUES_RESULT)  10
set fcgi(FCGI_UNKNOWN_TYPE)       11
 
# Value for requestId component of FCGI_Header
set fcgi(FCGI_NULL_REQUEST_ID)     0
 
# Mask for flags component of FCGI_BeginRequestBody
set fcgi(FCGI_KEEP_CONN)  1
 
# Values for role component of FCGI_BeginRequestBody
set fcgi(FCGI_RESPONDER)  1
set fcgi(FCGI_AUTHORIZER) 2
set fcgi(FCGI_FILTER)     3
 
# Values for protocolStatus component of FCGI_EndRequestBody
set fcgi(FCGI_REQUEST_COMPLETE) 0
set fcgi(FCGI_CANT_MPX_CONN)    1
set fcgi(FCGI_OVERLOADED)       2
set fcgi(FCGI_UNKNOWN_ROLE)     3
 
# Variable names for FCGI_GET_VALUES / FCGI_GET_VALUES_RESULT records
set fcgi(FCGI_MAX_CONNS)  "FCGI_MAX_CONNS"
set fcgi(FCGI_MAX_REQS)   "FCGI_MAX_REQS"
set fcgi(FCGI_MPXS_CONNS) "FCGI_MPXS_CONNS"
 
###############################################################################
# define fcgi state variables
 
set fcgi(requestId)     -1		;# current requestId in progress
set fcgi(origEnv)    [array names env]	;# list of orignal env names
set fcgi(listenSock)    -1		;# socket on which we listen
set fcgi(acceptCmd)     -1		;# command to accept new sock
set fcgi(newSock)       -1		;# new client socket
global fcgiNewSock 
set fcgiNewSock         -1		;# var to wait on Tcl socket
set fcgi(newClient)     ""		;# ip of client that connected
set fcgi(bufSize)       4096		;# stdout/stderr buffer size
set fcgi(notFcgi)	0		;# if app is running as normal CGI
 
 
###############################################################################
# define fcgi mgmt variables responses

set fcgi(fcgi_max_conns)     1		;# only one connection at a time
set fcgi(fcgi_max_reqs)      1		;# only one request at a time
set fcgi(fcgi_mpxs_conns)    0		;# don't multiplex connections
 
###############################################################################
# per request variables

# these are shown here as comments, actually set in FCGI_Accept
#set fcgi($requestId,sock)      -1	;# socket for connection
#set fcgi($requestId,env)       ""	;# environment
#set fcgi($requestId,paramsEof) 0	;# environment eof marker
#set fcgi($requestId,stdin)     ""	;# stdin buffer
#set fcgi($requestId,stdinEof)  0	;# stdin eof marker
#set fcgi($requestId,data)      ""	;# fcgi data buffer
#set fcgi($requestId,dataEof)   0	;# fcgi data eof marker
#set fcgi($requestId,dataRedir) 0	;# fcgi data redirected to stdin
#set fcgi($requestId,stdout)    ""	;# stdout buffer
#set fcgi($requestId,stdoutFlg) 0       ;# stdout written flag
#set fcgi($requestId,stderr)    ""	;# stderr buffer
#set fcgi($requestId,stderrFlg) 0       ;# stderr written flag
#set fcgi($requestId,keepConn)  0	;# keep connection 
#set fcgi($requestId,exitCode)  0	;# exit code
#set fcgi($requestId,role)      0	;# fcgi role

package require Tclx
# rename Tcl io commands so we can redefine them as fcgi aware
rename gets  _gets_tcl
rename read  _read_tcl
rename flush _flush_tcl
rename puts  _puts_tcl
rename eof   _eof_tcl


}   ;# end of namespace eval fcgi



###############################################################################
#
# replacement stdio procs to act on fcgi stdio/data stream as well as files
#
###############################################################################

###############################################################################
# fcgi "gets" wrapper proc

proc fcgi::gets {args} {
  variable fcgi
  set requestId $fcgi(requestId)
  if {$requestId == -1} {
    return [uplevel 1 fcgi::_gets_tcl $args]
  }
  if {[lindex $args 0] == "stdin"} {
    if {$fcgi($requestId,dataRedir) &amp;&amp; ! $fcgi($requestId,dataEof)} {
      set rc [processFcgiStream $fcgi($requestId,sock) $requestId "data"]
    } elseif {! $fcgi($requestId,stdinEof)} {
      set rc [processFcgiStream $fcgi($requestId,sock) $requestId "stdin"]
    } else {
      set rc 1	;# force "no error"
    }
    if {$rc &lt;= 0} {
      if {[llength $args] &gt; 1} {
	return 0
      } else {
        return ""
      }
    }
    set idx [string first \n $fcgi($requestId,stdin)]
    if {$idx == -1} {
      set idx [string length $fcgi($requestId,stdin)]
    }
    incr idx -1
    set msg [string range $fcgi($requestId,stdin) 0 $idx]
    incr idx 2
    set fcgi($requestId,stdin) [string range $fcgi($requestId,stdin) $idx end]
    if {[llength $args] &gt; 1} {
      uplevel 1 set [list [lindex $args 1]] [list $msg]
      return [string length $msg]
    } else {
      return $msg
    }
  } else {
    return [uplevel 1 fcgi::_gets_tcl $args]
  }
}


###############################################################################
# fcgi "read" wrapper proc

proc fcgi::read {args} {
  variable fcgi
  set requestId $fcgi(requestId)
  if {$requestId == -1} {
    return [uplevel 1 fcgi::_read_tcl $args]
  }

  # fill stdin or data buffer if stdin channel
  if {([lindex $args 0] == "stdin") || \
      ([lindex $args 0] == "-nonewline" &amp;&amp;[lindex $args 1] == "stdin")} {

    if {$fcgi($requestId,dataRedir) &amp;&amp; ! $fcgi($requestId,dataEof)} {
      set rc [processFcgiStream $fcgi($requestId,sock) $requestId "data"]
    } elseif {! $fcgi($requestId,stdinEof)} {
      set rc [processFcgiStream $fcgi($requestId,sock) $requestId "stdin"]
    } else {
      set rc 1	;# force "no error"
    }
    if {$rc &lt;= 0} {
      return ""
    }
  }

  if {[lindex $args 0] == "-nonewline"} {
    if {[lindex $args 1] == "stdin"} {
      # read from stdin buf until eof, chop last nl
      set msg [string trim $fcgi($requestId,stdin) \nl]
      returm $msg
    } else {
      return [uplevel 1 fcgi::_read_tcl $args]
    }
  } else  {
    if {[lindex $args 0] == "stdin"} {
      # read from stdin buf specific num of bytes
      if {[llength $args] &gt; 1} {
        set num 0
	scan [lindex $args 1] %d num
        set msg [string range $fcgi($requestId,stdin) 0 [expr $num - 1]]
        set fcgi($requestId,stdin) \
			       [string range $fcgi($requestId,stdin) $num end]
      } else {
	set msg $fcgi($requestId,stdin)
	set fcgi($requestId,stdin) ""
      }
      return $msg
    } else {
      return [uplevel 1 fcgi::_read_tcl $args]
    }
  }
}


###############################################################################
# fcgi "flush" wrapper proc

proc fcgi::flush {file} {
  variable fcgi
  set requestId $fcgi(requestId)
  if {$requestId == -1} {
    return [uplevel 1 fcgi::_flush_tcl $file]
  }
  if {$file == "stdout" || $file == "stderr"} {
    set num [string length $fcgi($requestId,$file)]
    while {$num &gt; 0} {
      set num [expr $num&lt;$fcgi(FCGI_MAX_LENGTH) ? $num : $fcgi(FCGI_MAX_LENGTH)]
      set msg [string range $fcgi($requestId,$file) 0 [expr $num - 1]]
      set fcgi($requestId,$file) \
			      [string range $fcgi($requestId,$file) $num end]
      if {$file == "stdout"} {
	set type $fcgi(FCGI_STDOUT)
      } else {
	set type $fcgi(FCGI_STDERR)
      }
      writeFcgiRecord $fcgi($requestId,sock) $fcgi(FCGI_VERSION_1) $type \
		      $requestId $msg
      set num [string length $fcgi($requestId,$file)]
    }
  } else {
    uplevel 1 fcgi::_flush_tcl $file
  }
  return ""
}



###############################################################################
# fcgi "puts" wrapper proc

proc fcgi::puts {args} {
  variable fcgi
  set requestId $fcgi(requestId)
  if {$requestId == -1} {
    return [uplevel 1 fcgi::_puts_tcl $args]
  }
  switch [llength $args] {
    1 {
      append fcgi($requestId,stdout) [lindex $args 0] \n
      set file stdout
    }
    2 {
      if {[lindex $args 0] == "-nonewline"} {
        append fcgi($requestId,stdout) [lindex $args 1]
        set file stdout
      } else {
        set file [lindex $args 0]
        if {$file == "stdout" || $file == "stderr"} {
          append fcgi($requestId,$file) [lindex $args 1] \n
        } else {
          uplevel 1 fcgi::_puts_tcl $args
        }
      }
    }
    default {
      set file [lindex $args 1]
      if {[lindex $args 0] == "-nonewline" &amp;&amp; \
	 ($file == "stdout" || $file == "stderr")} {
        append fcgi($requestId,$file) [lindex $args 2]
      } else {
        uplevel 1 fcgi::_puts_tcl $args
      }
    }
  }

  # set "written to" flag and check if flush needed
  if {[string compare $file "stdout"] == 0 || \
      [string compare $file "stderr"] == 0} {
    set fcgi($requestId,${file}Flg) 1
    if {[string length $fcgi($requestId,$file)] &gt; $fcgi(bufSize)} {
      flush $file
    }
  }
  return ""

}


###############################################################################
# fcgi "eof" wrapper proc

proc fcgi::eof {file} {
  variable fcgi
  set requestId $fcgi(requestId)
  if {$requestId == -1} {
    return [uplevel 1 fcgi::_eof_tcl $file]
  }
  if {$file == "stdin"} {
    if {[string length $fcgi($requestId,$file)] == 0 &amp;&amp; \
	$fcgi($requestId,stdinEof)} {
      return 1
    } else {
      return 0
    }
  } else {
    return [uplevel 1 fcgi::_eof_tcl $file]
  }
}


###############################################################################
#
# fcgi support routines
#
###############################################################################

 
###############################################################################
# read fcgi record
 
proc fcgi::readFcgiRecord {sock} {
  variable fcgi
  set msg ""
 
  while {[string length $msg] != $fcgi(FCGI_HEADER_LEN)} {
    append msg \
	[_read_tcl $sock [expr $fcgi(FCGI_HEADER_LEN) - [string length $msg]]]
  }
 
  set version         0
  set type            0
  set requestId       0
  set contentLength   0
  set paddingLength   0
  set reserved        0
 
  # read the header
  binary scan $msg ccSScc version type requestId contentLength \
                          paddingLength reserved
 
  # convert everything to unsigned int values
  set version       [expr ($version       + 0x100)   % 0x100]
  set type          [expr ($type          + 0x100)   % 0x100]
  set requestId     [expr ($requestId     + 0x10000) % 0x10000]
  set contentLength [expr ($contentLength + 0x10000) % 0x10000]
  set paddingLength [expr ($paddingLength + 0x100)   % 0x100]
 
  # read msg content
  set content ""
  while {[string length $content] != $contentLength} {
    append content \
	[_read_tcl $sock [expr $contentLength - [string length $content]]]
  }
 
  # read msg padding
  set padding ""
  while {[string length $padding] != $paddingLength} {
    append padding \
	[_read_tcl $sock [expr $paddingLength - [string length $padding]]]
  }
 
  return [list $version $type $requestId $contentLength $content]
}

 
###############################################################################
# write fcgi record
 
proc fcgi::writeFcgiRecord {sock version type requestId content} {

  set contentLength [string length $content]
  # ccSScc = version type requestId contentLength padding reserved

  catch {
    _puts_tcl -nonewline $sock \
	   [binary format ccSScc $version $type $requestId $contentLength 0 0]
    _puts_tcl -nonewline $sock $content
    _flush_tcl $sock
  }

}
 
 
###############################################################################
# scan fcgi request body
#   input: message string of type FCGI_BEGIN_REQUEST
 
proc fcgi::scanFcgiRequestBody {msg} {
  set role   0
  set flags  0
  # last 5 bytes are reserved
  binary scan $msg Scc5 role flags reserved
  set role  [expr ($role  + 0x10000) % 0x10000]
  set flags [expr ($flags + 0x100)   % 0x100]
  return [list $role $flags]
}
 

###############################################################################
# format fcgi end request response
 
proc fcgi::formatFcgiEndRequest {appStatus protocolStatus} {
  return [binary format Icc3 $appStatus $protocolStatus {0 0 0}]
}
 
 
###############################################################################
# format fcgi unknown type response
 
proc fcgi::formatFcgiUnknownType {type} {
  return [binary format cc7 $type {0 0 0 0 0 0 0}]
}
 

###############################################################################
# scan fcgi name value pair
 
proc fcgi::scanFcgiNameValue {msg} {
 
  # get name len
  set nlen 0
  binary scan $msg c nlen
  set nlen [expr ($nlen + 0x100) % 0x100]
  if {$nlen &gt; 127} {
    binary scan $msg I nlen
    set nlen [expr $nlen &amp; 0x7fffff]
    set nlenLen 4
  } else {
    set nlenLen 1
  }
 
  # get value len
  set vlen 0
  binary scan $msg "x${nlenLen}c" vlen
  set vlen [expr ($vlen + 0x100) % 0x100]
  if {$vlen &gt; 127} {
    binary scan $msg "x${nlenLen}I" vlen
    set vlen [expr $vlen &amp; 0x7fffff]
    set vlenLen 4
  } else {
    set vlenLen 1
  }
 
  # get name and value
  set fmt [format x%dx%da%da%d $nlenLen $vlenLen $nlen $vlen]
  set name  ""
  set value ""
  binary scan $msg $fmt name value
  set totLen [expr $nlenLen + $vlenLen + $nlen + $vlen]
  return [list $totLen $name $value]
}


###############################################################################
# format fcgi name value pair
 
proc fcgi::formatFcgiNameValue {name value} {
  set nlen [string length $name]
  set vlen [string length $value]
  if {$nlen &gt; 127} {
    set nlenFmt I
    set nlen [expr $nlen | 0x80000000]
  } else {
    set nlenFmt c
  }
  if {$vlen &gt; 127} {
    set vlenFmt I
    set vlen [expr $vlen | 0x80000000]
  } else {
    set vlenFmt c
  }
  set fmt [format %s%sa%da%d $nlenFmt $vlenFmt $nlen $vlen]
  return [binary format $fmt $nlen $vlen $name $value]
}


###############################################################################
# respond to mgmt record requests
 
proc fcgi::respondFcgiMgmtRecord {s msg} {
  variable fcgi
  set requestId $fcgi(requestId)

  set reply ""

  while {[string length $msg] &gt; 0} {
    set nameValue [scanFcgiNameValue $msg]
    set totLen [lindex $nameValue 0]
    set name   [lindex $nameValue 1]
    set value  [lindex $nameValue 2]
    set msg [string range $msg $totLen end]

    # "open" style of switch command
    switch -- $name \
      $fcgi(FCGI_MAX_CONNS)  {
	 append reply [formatFcgiNameValue $name $fcgi(fcgi_max_conns)]    
      } \
      $fcgi(FCGI_MAX_REQS) {
	 append reply [formatFcgiNameValue $name $fcgi(fcgi_max_reqs)]    
      } \
      $fcgi(FCGI_MPXS_CONNS) {
	 append reply [formatFcgiNameValue $name $fcgi(fcgi_mpxs_conns)]    
      } \
      default {
      }
    
  }

  if {[string length $reply] &gt; 0} {
     writeFcgiRecord $fcgi($requestId,sock) $fcgi(FCGI_VERSION_1) \
	     $fcgi(FCGI_GET_VALUES_RESULT) 0 $reply 
  }
}


###############################################################################
# process fcgi header / new request
# returns list: requestId role flags - new request 
#        {-1 0 0} - server tried to multiplex request
#        { 0 0 0} - socket closed 

proc fcgi::getFcgiBeginRequest {sock} {
  variable fcgi

  set type -1
  while {$type != $fcgi(FCGI_BEGIN_REQUEST)} {
    if {[catch {set msg [readFcgiRecord $sock]}]} {
      # read error
      return {0 0 0}
    }
    set version       [lindex $msg 0]
    set type          [lindex $msg 1]
    set requestId     [lindex $msg 2]
    set contentLength [lindex $msg 3]
    set content       [lindex $msg 4]
    if {$type == $fcgi(FCGI_BEGIN_REQUEST)} {
      set msg [scanFcgiRequestBody $content]
      set role  [lindex $msg 0]
      set flags [lindex $msg 1]
      return [list $requestId $role $flags]
    } elseif {$requestId == 0 || $type == $fcgi(FCGI_GET_VALUES)} {
	respondFcgiMgmtRecord $sock $content
    } else {
      writeFcgiRecord $sock $version $fcgi(FCGI_UNKNOWN_TYPE) 0 \
						 [formatFcgiUnknownType $type]
      return {-1 0 0}
    }
  }
}


###############################################################################
# process fcgi connections
# returns 1 - "waitfor" stream completed
#        -1 - server tried to multiplex request or abort request
#         0 - socket closed 

proc fcgi::processFcgiStream {sock requestId waitfor} {
  variable fcgi

  switch -- $waitfor {
    params {set waitfor fcgi($requestId,paramsEof)}
    stdin  {set waitfor fcgi($requestId,stdinEof)}
    data   {set waitfor fcgi($requestId,dataEof)}
    default {return -1}
  }

  while {! [set $waitfor]} {

    if {[catch {set msg [readFcgiRecord $sock]}]} {
      # read error
      return 0
    }
    set version       [lindex $msg 0]
    set type          [lindex $msg 1]
    set requestId     [lindex $msg 2]
    set contentLength [lindex $msg 3]
    set content       [lindex $msg 4]

    if {$requestId == 0} {
      respondFcgiMgmtRecord $sock $content
      continue
    }

    if {$requestId != $fcgi(requestId)} {
      writeFcgiRecord $sock $version $fcgi(FCGI_END_REQUEST) $requestId \
		 [formatFcgiEndRequest 0 $fcgi(FCGI_CANT_MPX_CONN)]
      return -1
    }

    # "open" style of switch command
    switch -- $type \
      $fcgi(FCGI_PARAMS) {
	if {$contentLength == 0} {
          set fcgi($requestId,paramsEof) 1
	} else {
	  while {[string length $content] &gt; 0} {
            set msg [scanFcgiNameValue $content]
            lappend fcgi($requestId,env)  [lindex $msg 1] [lindex $msg 2]
	    set content [string range $content [lindex $msg 0] end]
	  }
	}
      } \
      $fcgi(FCGI_STDIN) {
	if {$contentLength == 0} {
	  set fcgi($requestId,stdinEof) 1
	} else {
	  if {!$fcgi($requestId,dataRedir)} {
	    append fcgi($requestId,stdin) $content
	  } 
	}
      } \
      $fcgi(FCGI_DATA)   {
	if {$contentLength == 0} {
	  set fcgi($requestId,dataEof) 1
	  if {$fcgi($requestId,dataRedir)} {
	    set fcgi($requestId,stdin) $fcgi($requestId,data)
	    set fcgi($requestId,stdinEof) 1
	  }
	} else {
	  append fcgi($requestId,data) $content
	}
      } \
      $fcgi(FCGI_GET_VALUES) {
	respondFcgiMgmtRecord $sock $content
      } \
      $fcgi(FCGI_ABORT_REQUEST) {
        writeFcgiRecord $sock $fcgi(FCGI_VERSION_1) \
             $fcgi(FCGI_END_REQUEST) $requestId \
             [formatFcgiEndRequest 0 $fcgi(FCGI_REQUEST_COMPLETE)]
	
	return -1
      } \
      $fcgi(FCGI_END_REQUEST) - \
      $fcgi(FCGI_UNKNOWN_TYPE) - \
      $fcgi(FCGI_STDOUT) - \
      $fcgi(FCGI_STDERR) {
	# ignore these packets
      } \
      default {
	# send back unknown type
        writeFcgiRecord $sock $version $fcgi(FCGI_UNKNOWN_TYPE) $requestId \
						 [formatFcgiUnknownType $type]
      } 
    # end of switch 
  }

  return 1
}


###############################################################################
# set up env for new connection

proc fcgi::setupFcgiEnv {requestId} {
  variable fcgi
  global env
  
  # unset all but orignal env names
  foreach {name} [array names env] {
    if {[lsearch $fcgi(origEnv) $name] == -1} {
      unset env($name)
    }
  }

  # add in env for this fcgi connection
  foreach {name value} $fcgi($requestId,env) {
    set env($name) $value
  }
}


###############################################################################
# clean up per request fcgi variables

proc fcgi::cleanUpFcgi {requestId} {
  variable fcgi

  catch {unset fcgi($requestId,sock)     }
  catch {unset fcgi($requestId,env)      }
  catch {unset fcgi($requestId,paramsEof)}
  catch {unset fcgi($requestId,stdin)    }
  catch {unset fcgi($requestId,stdinEof) }
  catch {unset fcgi($requestId,data)     }
  catch {unset fcgi($requestId,dataEof)  }
  catch {unset fcgi($requestId,dataRedir)}
  catch {unset fcgi($requestId,stdout)   }
  catch {unset fcgi($requestId,stdoutFlg)}
  catch {unset fcgi($requestId,stderr)   }
  catch {unset fcgi($requestId,stderrFlg)}
  catch {unset fcgi($requestId,keepConn) }
  catch {unset fcgi($requestId,exitCode) }
  catch {unset fcgi($requestId,role)     }

}


###############################################################################
# reset of cgi.tcl environment

proc fcgi::resetCgiEnv {} {

  # if also using cgi.tcl, get the _cgi global array, and save beginning values.
  # cgi.tcl uses the _cgi array to save state information, which needs to
  # be reset on each FCGI_Accept call

  global _cgi 
  variable fcgi_cgi

  if {[array exists _cgi]} {
    # try to use cgi.tcl reset environment, otherwise do it ourselves
    if {[catch {cgi_reset_env}]} {
      # set _cgi back to beginning values
      if {![array exists fcgi_cgi]} {
	array set fcgi_cgi [array get _cgi]
      }
      catch {unset _cgi}
      array set _cgi [array get fcgi_cgi]
      # unset other _cgi_xxxx vars 
      # untouched are: _cgi_link _cgi_imglink _cgi_link_url
      set cgi_vars {_cgi_uservar _cgi_cookie _cgi_cookie_shadowed _cgi_userfile}
      foreach v $cgi_vars {
        global $v
        catch {unset $v}
      }
    }
  }

}


###############################################################################
#
# application interfaces
#
###############################################################################


###############################################################################
# accept a new fcgi connection, this is the primary call from the application

proc fcgi::FCGI_Accept {} {
  variable fcgi

  global env
  set requestId $fcgi(requestId)

  # if we started with stdin as a real stdin, then fail second time around 
  if {$fcgi(notFcgi)} {
    return -1
  }
  
  # flush and pending request
  if {$fcgi(requestId) != -1} {
    FCGI_Finish
  }

  # execute the accept command, either 'fcgiTclxAccept' or 'fcgiSockAccept'
  set sock [$fcgi(acceptCmd) $fcgi(listenSock)]

  # if we get a null back from accept, means we're running as plain CGI
  if {[string length $sock] == 0} {
    # set to fail on second time
    set fcgi(notFcgi) 1

    # set role as responder
    set env(FCGI_ROLE) RESPONDER

    return 0
  }

  # get the begin request message
  set newFcgi [getFcgiBeginRequest $sock]

  if {[string compare $newFcgi 0] == 0} {
    return -1
  } elseif {[string compare $newFcgi -1] == 0} {
    return  -1
  }
  set requestId [lindex $newFcgi 0]
  set role      [lindex $newFcgi 1]
  set flags     [lindex $newFcgi 2]

  if {$requestId == -1} {
    return -1
  }

  set fcgi(requestId)            $requestId
  set fcgi($requestId,sock)      $sock	;# socket for connection
  set fcgi($requestId,env)       ""	;# environment
  set fcgi($requestId,paramsEof) 0	;# environment eof marker
  set fcgi($requestId,stdin)     ""	;# stdin buffer
  set fcgi($requestId,stdinEof)  0	;# stdin eof marker
  set fcgi($requestId,data)      ""	;# fcgi data buffer
  set fcgi($requestId,dataEof)   0	;# fcgi data eof marker
  set fcgi($requestId,dataRedir) 0	;# fcgi data redirected to stdin
  set fcgi($requestId,stdout)    ""	;# stdout buffer
  set fcgi($requestId,stdoutFlg) 0	;# stdout written flag
  set fcgi($requestId,stderr)    ""	;# stderr buffer
  set fcgi($requestId,stderrFlg) 0	;# stderr written flag
  set fcgi($requestId,keepConn)  $flags	;# keep connection 
  set fcgi($requestId,exitCode)  0	;# exit code
  set fcgi($requestId,role)      $role	;# fcgi role

  # get fcgi params streams until no more params
  set rc [processFcgiStream $sock $requestId "params"]

  if {$rc &lt;= 0} {
    cleanUpFcgi $requestId
    return -1
  }

  setupFcgiEnv $requestId

  # "open" style of switch command
  switch -- $fcgi($requestId,role) \
    $fcgi(FCGI_RESPONDER)  {
      set env(FCGI_ROLE) RESPONDER
    } \
    $fcgi(FCGI_AUTHORIZER) {
      set env(FCGI_ROLE) AUTHORIZER
    } \
    $fcgi(FCGI_FILTER)     {
      set env(FCGI_ROLE) FILTER
    } \
    default {
      set env(FCGI_ROLE) ""
    }
  # end of switch


  # cause cgi.tcl to be sourced, if not already sourced, and reset cgi.tcl
  catch {cgi_lt}
  resetCgiEnv

  return 0
}


###############################################################################
# finish fcgi connection

proc fcgi::FCGI_Finish {} {
  variable fcgi
  set requestId $fcgi(requestId)

  # write stdout and stderr bufs
  foreach {file type} {stdout FCGI_STDOUT stderr FCGI_STDERR} {
    if {$fcgi($requestId,${file}Flg)} {
      flush $file
      # send zero length as eof
      writeFcgiRecord $fcgi($requestId,sock) $fcgi(FCGI_VERSION_1) \
	      $fcgi($type) $requestId ""
    }
  }

  # write end request
  writeFcgiRecord $fcgi($requestId,sock) $fcgi(FCGI_VERSION_1) \
    $fcgi(FCGI_END_REQUEST) $requestId \
    [formatFcgiEndRequest $fcgi($requestId,exitCode) \
		          $fcgi(FCGI_REQUEST_COMPLETE)]

  # check to teardown socket
  if {! ($fcgi($requestId,keepConn) &amp; $fcgi(FCGI_KEEP_CONN) )} {
    close $fcgi($requestId,sock)
  }

  # clean up
  cleanUpFcgi $requestId

  set fcgi(requestId)  -1
  
}


###############################################################################
# set exit status for fcgi 

proc fcgi::FCGI_SetExitStatus {status} {
  variable fcgi

  set requestId $fcgi(requestId)
  set fcgi($requestId,exitCode) $status
  return ""
}


###############################################################################
# start filter data

proc fcgi::FCGI_StartFilterData {} {
  variable fcgi

  set requestId $fcgi(requestId)
  set fcgi($requestId,stdin)    $fcgi($requestId,data)
  set fcgi($requestId,stdinEof) $fcgi($requestId,dataEof)
  set fcgi($requestId,dataRedir) 1
  return ""
}


###############################################################################
# set buffer size, valid sizes: 0 to FCGI_MAX_LENGTH

proc fcgi::FCGI_SetBufSize {size} {
  variable fcgi

  set newSize -1
  catch {scan $size %d newSize}
  if {$newSize &gt;= 0 &amp;&amp; $newSize &lt;= $fcgi(FCGI_MAX_LENGTH)} {
    set fcgi(bufSize) $newSize
  }
  return $fcgi(bufSize)
}




###############################################################################
#
# start up fcgi processing
#
###############################################################################

namespace eval fcgi {

variable fcgi
global env


###############################################################################
# procs to handle native Tcl socket accepts &amp; Tclx accepts
#

# callback proc from Tcl's 'socket -server' 
proc fcgiAccept {sock client port} {
  global fcgiNewSock
  variable fcgi
  set fcgi(newClient) $client
  set fcgiNewSock $sock
  update
}

# blocking 'accept' for Tcl sockets
proc fcgiSockAccept {sock} {
  global fcgiNewSock
  variable fcgi
  vwait fcgiNewSock
  set fcgi(newSock) $fcgiNewSock
  fconfigure $fcgi(newSock) -translation binary
  return $fcgiNewSock
}

# blocking 'accept' for TclX sockets 
proc fcgiTclxAccept_ {sock} {
  variable fcgi
  set fcgi(newSock)   ""
  set fcgi(newClient) ""
  # watch for failure, if so then we probably started with stdin = real stdin
  if {[catch {set fcgi(newSock) [server_accept $sock]}] == 0} {
    # we got a good accept, change channel to binary
    fconfigure $fcgi(newSock) -translation binary
    catch {set fcgi(newClient) [lindex [fconfigure -socket $fcgi(newSock)] 0]}
  }
  return $fcgi(newSock)
}

proc fcgiTclxAccept {sock} {
    variable fcgi
    set fcgi(newSock)   ""
    set fcgi(newClient) ""
    # watch for failure, if so then we probably started with stdin = real stdin
    if {[catch {
	fconfigure $sock -translation binary }] == 0} {

	set fcgi(newSock) $sock
	# if we got a good fconfigure, change channel to binary	
	#fconfigure $fcgi(newSock) -translation binary
	catch {set fcgi(newClient) [lindex [fconfigure fcgi(newSock) -socketname] 0]}
    }

    return $fcgi(newSock)

}


###############################################################################
#    look for port on which to listen, either as argument(-port) or env(PORT)
#    if neither, then use file descriptor 0 as server port

set port -1

# check for argv "-port xxx" first
global argv argc
for {set i 0} {$i &lt; $argc} {incr i} {
  if {[string compare [lindex $argv $i] "-port"] == 0} {
    incr i
    scan [lindex $argv $i] %d port
  }
}

# next, check env(PORT)
if {$port &lt; 0} {
  if {[info exists env(PORT)]} {
    scan $env(PORT) %d port
  }
}


# if port was found, then open a server socket on which to listen
# if no port was found, assume we started with as a forked process with 
# stdin = unix domain socket from apache's mod_fastcgi.

if {$port &lt; 0} {
  # we use the fine Tclx extension for this style of connection
  package require Tclx
  set fcgi(listenSock) stdin
  set fcgi(acceptCmd)  fcgiTclxAccept

#   if {[catch {set fcgi(newSock) [server_accept $sock]}] == 0} {

} else {
  set fcgi(listenSock) [socket -server fcgiAccept $port]
  set fcgi(acceptCmd)  fcgiSockAccept
}



# export applications and io wrapper commands
namespace export FCGI_Accept FCGI_Finish FCGI_SetExitStatus \
		 FCGI_StartFilterData FCGI_SetBufSize
namespace export gets read flush puts eof

#  rename gets  _gets_tcl
#   rename read  _read_tcl
#   rename flush _flush_tcl
#   rename puts  _puts_tcl
#   rename eof   _eof_tcl

 
}   ;# end of namespace eval fcgi


# make the application use fcgi wrappers for these io commands
# hmm.  If I'm running under CGI,  I don't want to do anything.
#namespace import -force fcgi::gets
#namespace import -force fcgi::read
proc curry {new args} {eval [list interp alias {} $new {}] $args}

curry gets fcgi::gets
curry read fcgi::read 

namespace import -force fcgi::flush
namespace import -force fcgi::puts
namespace import -force fcgi::eof

# import the application fcgi commands
namespace import fcgi::FCGI_Accept 
namespace import fcgi::FCGI_Finish 
namespace import fcgi::FCGI_SetExitStatus 
namespace import fcgi::FCGI_StartFilterData
namespace import fcgi::FCGI_SetBufSize
#package require Fcgi

###### END FAST CGI CODE ##################################


###### BEGIN FCGI-&gt;OPEN ACS OVERLOADED COMMANDS #######################

package require cgi


proc cgi_import_list {} {
    global _cgi
    ns_log debug "cgi_import_list: [array names _cgi]"
    
    if [info exists _cgi(uservars)] {
	return $_cgi(uservars)
    }

    return [list]
}

cgi_debug -on

source pnsd-init.tcl
::pnsd::source_openacs


::pnsd::source_openacs
set ::nstcl::debug_p 1


rename ns_conn ns_conn_default

#FastCGI version -&gt; use CGI globals
proc ns_conn { cmd args } { 
    global env
    #ns_log debug "fcgi::ns_conn called with $cmd"
    switch $cmd {
	"host" { 

	    if { [info exists env(HTTP_HOST)] &amp;&amp; $env(HTTP_HOST) != "" } { 
		ns_log debug "\[ns_conn]Host $env(HTTP_HOST)"
		return $env(HTTP_HOST) 
	    } else {
		ns_log debug "\[ns_conn]Host -&gt;[info hostname]"
		return [info hostname]
	    }
	}
	"port" { 
	    if [info exists env(SERVER_PORT)] { 
		return $env(SERVER_PORT) 
	    } 
	    #otherwise we're running command line - use default
	}
	"peeraddr" { return $env(REMOTE_ADDR) }
	"form" { 
	    ns_log debug "ns_connn form says hey [cgi_import_list] there "
	    global _cgi _cgi_uservar
	    set form [ns_set create formdata]	  	    
	    
	    set lst [list new] ;  #err ... why did I do this?
	    lappend lst [cgi_import_list]
	    if [info exists _cgi(uservars) ] {
		ns_log debug "Notice _cgi(uservars) "
		foreach varname $_cgi(uservars) {		    
		    ns_log debug "putting ... $varname into $form "
		    #Is this a list or not?
		    if {-1 == [lsearch $_cgi(uservars,autolist) $varname]} {
			ns_set put $form $varname $_cgi_uservar($varname)
		    } else {
			foreach element [split $_cgi_uservar($varname)] {
			    ns_set put $form $varname $element
			}
		    }

		}
	    }
	    return $form
	}
	"query" { 
	    return ""
	    if { [ info exists env(QUERY_STRING) ] } { 
		return $env(QUERY_STRING) 
	    } else { 
		return "" 
	    }	
	}				
	"protocol" { return $env(SERVER_PROTOCOL) }
	"url" { 
#	    ns_log Debug "url returning $env(SCRIPT_URL) "
	    return $env(SCRIPT_URL)
	}
	"version" { return "1.0" }
	"authuser" { return $env(REMOTE_USER) }
	"contentlength" { return 0 }
	"isconnected" { return 1 }
	
    }
    
    return [ ns_conn_default $cmd $args ]

}

rename ns_config ns_config_default

proc ns_config { section item args} {
    
    switch $item {
	"Hostname" { return [ns_conn host ] }
	"HomeURL" { return "http://[ns_config "" Hostname]/pvt" }
    }

    return [ns_config_default $section $item $args]



}


namespace eval pnsd {

#stolen from tclhttpd
    variable Httpd_Errors
    array set Httpd_Errors {
	200 {Data follows}
	204 {No Content}
	302 {Found}
	304 {Not Modified}
	400 {Bad Request}
	401 {Authorization Required}
	403 {Permission denied}
	404 {Not Found}
	408 {Request Timeout}
	411 {Length Required}
	419 {Expectation Failed}
	500 {Server Internal Error}
	501 {Server Busy}
	503 {Service Unavailable}
	504 {Service Temporarily Unavailable}
    }

    proc ::pnsd::http_head { status type } {
	::pnsd::dump_headers
	cgi_content_type $type 
    }
    

}

#TODO: move to buffered output using fcopy directly  (won't matter so much if caching proxy implemented first)
#doesn't currently work with binary content.

proc ns_getform { } {

    ns_log debug "Entering new ns_getform [cgi_import_list]"

#    ns_log debug [parray env]
    set form [ns_conn form]
    ns_log debug "printing form ...[ns_set print $form] "

    return $form

    global _cgi_uservar

    set form [ ns_set create cgivars ]

#     foreach { pair } [ split $::pnsd::querystring &amp; ] {
# 	set keyval  [ split $pair = ]
# 	set key [lindex $keyval 0]
# 	set val [ncgi::decode [lindex $keyval 1]]
# 	ns_set put $form $key $val	
#     }

    foreach key [ array names _cgi_uservar ] {
	ns_set put $form $key $_cgi_uservar($key)
    }

    ns_set put $form sdsdsds dsdsdsdsds

    ns_log notice "ns_getform returning\n [ns_set print $form]"    

    return $form

}

proc ns_return { status type string } {
    ns_log notice "jjsX: ns_return: status $status type=$type length=[string length $string] "
    
#    set cmd [list ::pnsd::http_head $status $type ]
#[list cgi_status $status; ::pnsd::dump_headers; cgi_content_type $type ]
    
    ns_log debug "ns_return: Command $cmd"
    cgi_http_head [list ::pnsd::http_head $status $type ]
    
    set ::pnsd::__http_mime $type
    set ::pnsd::__http_stream $string

    return 
}


proc ns_returnnotfound { } {

    global _cgi
#    unset _cgi(http_head_in_progress)
#    ns_log debug "ns_rnf Status : [parray _cgi ]"

    cgi_http_head { cgi_status 404 "Not Found"}

    set ::pnsd::http_done_p "t"

}

#jjs: This will do for low-performing serving of  HTML files for now.  
# I should really do an fcopy to the fcgi socket,  but probably better still would be 
# to use a caching reverse proxy and maintain accurate cache/modified headers

#jjs: This will do for low-performing serving of  HTML files for now.  
#I should really do an fcopy to the fcgi socket

proc ns_returnfile { status type file } {
    
#use ::fileutil::cat $source ?
    set chan [open $file r]
    set text [read -nonewline $chan]
    close $chan

    ns_log debug "ns_returnfile $status $type $file "
    ns_log debug "ns_returnfile: [string range $text 0 100] "

    ns_return $status $type $text

}

ad_proc ns_startcontent { {-type "text/html"} }  {
    
    ns_log "ns_startcontent called "   
    cgi_http_head [list ::pnsd::http_head 200 $type ]

    set ::pnsd::__http_mime $type

}

proc ns_returnredirect { location } {
    ns_log debug "redirecting to $location"
#1)
global _cgi
set _cgi(http_head_in_progress) t
#    cgi_redirect $location
#2)
    ::pnsd::dump_headers
    cgi_status 302 Redirected
    cgi_puts "Uri: $location"
    cgi_puts "Location: $location"    
#3)

    #make sure I get cookies
#     variable headers [ad_conn outputheaders]
#     ns_set put $headers "Status" "302 Redirected"
#     ns_set put $headers "Uri" $location
#     ns_set put $headers "Location" $location
    cgi_puts ""

    set ::pnsd::http_done_p t
    unset _cgi(http_head_in_progress) 
    set _cgi(http_head_done) 1

}


 



# I should allow this take a code arg to uplevel
proc ::pnsd::dump_headers {} {
    variable headers
    set headers [ad_conn outputheaders]
    for {set i 0} {$i &lt; [ns_set size $headers]} {incr i} {
	ns_log debug  "header-dump [ns_set key $headers $i]: [ns_set value $headers $i]"	    
	cgi_puts "[ns_set key $headers $i]: [ns_set value $headers $i]"	    
    }

}

proc ad_return_template {args} { 
    ns_log debug "ad_return_template called with $args (no-op)"
}



#BROKEN PROCS
#Since I'm only going to deal with HTML content right now,  this is not
#important
proc util_WriteWithExtraOutputHeaders { args } { 
    # NOT YET WORKING
    ns_log warn "no-op: util_WriteWithExtraOutputHeaders called"
}
set ::pnsd::invariants(util_WriteWithExtraOutputHeaders) 1

#proc ad_table {args}  {
#    ns_log debug "ad_table called with $args (no-op)"
#}



proc ns_queryget { key } {     
    return [ ns_set value [ ns_getform ] $key ]
} 


#curry ns_share global


################### FASTCGI LISTENER ########################
#source c:/Tcl/lib/Fcgi04/fcgi.tcl
#package require Fcgi

set count 0 
ns_log Notice "OpenACS/fastCGI waiting for requests..."

ns_set truncate $::pnsd::headers_id 0
while {[FCGI_Accept] &gt;= 0 } {
#    ns_log Notice "FCGI_Accept begun."
    ns_set update $::pnsd::headers_id Host [ns_conn host]
    ns_set update $::pnsd::headers_id Referer ""
    if [info exists env(HTTP_COOKIE)] {
	ns_set update $::pnsd::headers_id COOKIE $env(HTTP_COOKIE)
    }
#    ns_log debug "incomdingg......."
    #ns_log Debug "Headers comming in : [ns_set print [ad_conn headers]]"

    #grab code I'm testing,  so I don't have to restart server.
    if [file exists new.tcl] {
	source new.tcl
    }
    cgi_eval {
    #set up cgi var's
	cgi_input    
	incr count

	rp_filter "because i said so"

#	ns_log debug "VARS: [info vars]"
#	ns_log debug "VARS:2 [info locals]"

	
	#If I issued a redirect or 404 skip rp_handler.	
	if {$::pnsd::http_done_p != "t" } {
	    rp_handler

	    if  { $::pnsd::__http_stream != "" }  {
		ns_log debug "Now calling dump_headers"		
		cgi_http_head {::pnsd::dump_headers}
		cgi_puts ""
		cgi_puts $::pnsd::__http_stream
#		parray env
#		::pnsd::dump_headers

	    }     

	}



    }

    #clear out any ACS legacy globals...
    pnsd::reset_connection


}

#### NB: Don't define anything below this line... you won't get there.</t>
<t tx="T9">@ignore
#!/usr/bin/tclsh 
# pnsd-init.tcl - Initialize all the files that support tclsh abstraction of openacs
# 	$Id: pnsd.leo,v 1.4 2004/01/21 08:02:18 john Exp $	
#

&lt;&lt;copyright&gt;&gt;

#put here to make tclpro happy
#package require htmlparse
 
package require nstcl 
namespace import ::nstcl::*    

#the beginnings of namespace definition
namespace eval ::pnsd { 
    variable root
    variable querystring ""
    variable url ""
    variable __http_stream ""
    variable redirect_p "f"
    variable http_done_p "f"
    variable home ""
    variable invariants 



    set ::pnsd::home [string trimright [file dirname [info script]] ./]
    set ::pnsd::home [file join [pwd] $home]
    
#    set ::pnsd::root [file join $home openacs-4]
    set ::pnsd::root [file join [lindex [file split [pwd]] 0 ] temp/openacs-4]

#JJS: This is a hack for needed now bc openacs allows empty src attributes in master tags, but nstcl does not
    set ::nstcl::template::default_master [file join $::pnsd::root www/default-master]



    variable log_stream ""
    set ::pnsd::log_stream [open [file join $::pnsd::home log.txt] w ]

#makes emacs happy,  but comment it out if you read logs in notepad
    fconfigure $::pnsd::log_stream -buffering none -translation lf 

    # Set up output Headers
    variable output_headers_id
    set output_headers_id [ns_set create outputheaders]
    
    # Set up headers ... 
    variable headers_id
    set headers_id [ns_set create headers]
    ns_set update $headers_id Host [info hostname]
    ns_set update $headers_id Referer ""




# proc ad_proc { args } {
#     #find proc name first argument that's not a flag
# #    puts "args $args"
# #    puts "lsearch [lsearch -regexp  $args {^\w}]"
#     set procname [lindex $args [lsearch -regexp  $args {^\w}]]
# #    puts $procname
#     #return if proc name in list
#     if {[info exists ::pnsd::invariants($procname)] } { 
# 	puts "skipping predefined nstcl proc"
# 	return
#     }

#     eval [subst "::nstcl::ad_proc $args"]
        
# }

array set ::pnsd::invariants {}
foreach name  {
    db_string 
    db_list
    db_list_of_lists
    db_list_of_ns_sets
    db_multirow
    db_0or1row
    db_1row
    db_quote
    db_transaction
    db_abort_transaction
    db_continue_transaction
    db_name
    db_dml
    db_exec
    db_exec_plpgsql
    db_exec_plsql
    db_foreach
    db_abort_transaction_p
    multirow
    template::multirow
    template::adp_eval
    template::adp_prepare
    template::adp_init
    template::adp_abort
    template::adp_compile

    template::adp_parse
    adp_parse_ad_conn_file

} { 
    set ::pnsd::invariants($name) 1 
}

proc source_openacs {} {

    #need to make sure openacs doesn't redefine nstcl database and templating commands
    uplevel 1 { 
	rename proc _proc

	_proc ::proc {
	    name 
	    arglist 
	    body
	} {
	    #array set mutable_procs { ::ad_arg_parser }
	    if {[info exists ::pnsd::invariants($name)]} { 
		#puts "NOT defining $name ... already defined"

		return
	    }
	    
	    if [llength [info commands ::nstcl::$name]] {  
		if {![string match $name "::ad_arg_parser"] 
		    &amp;&amp; ![string match $name "ad_proc"]} { 
		    #puts "NOT redefining $name"; 
		    return 
		} 
		#puts "redefining $name"
	    } 
	    if [llength [info commands $name]] {  	    
		#puts "Redefining existing command $name"
	    }

	    #   puts "defining $name"
	    uplevel 1 [list _proc $name $arglist $body]
	    #     uplevel 1 [list trace add execution $name enterstep [list ::proc_start $name]]
	    
	}
    }


    
    uplevel #0 "source $::pnsd::root/tcl/0-acs-init.tcl"
    
#    uplevel 1 {
#	rename proc ""
#	rename _proc proc
#    }


}
#source_openacs

}   
#namespace



#Overload nstcl database routines to make them QD-aware
foreach proc { 
    db_dml 
    db_string 
    db_foreach     
    db_0or1row
    db_1row
    db_list
    db_lst_of_lists
} {
    
   proc $proc { statement_name pre_sql args } "
        set full_statement_name statement_name
	if \[catch {
            set full_statement_name \[db_qd_get_fullname \$statement_name]
            set sql \[ db_qd_replace_sql \$full_statement_name \$pre_sql] } ] {
	    set sql \$pre_sql
	}
	
#        if {! \[string equal \$pre_sql \$sql] } {
#          puts \[list \"QD interceptifier:\" \$statement_name \$full_statement_name \$pre_sql \$sql  ]
#        }
#        puts \" Full Statement Name: \$full_statement_name\"
        set cmd [concat \[list ::nstcl::$proc  \$statement_name \$sql] \$args]
#        puts \$cmd
	uplevel 1 \[concat \[list ::nstcl::$proc \$statement_name \$sql] \$args]
    "

}


::nstcl::ad_proc db_multirow {-local:boolean 
    -append:boolean 
    -extend:optional 
    var_name statement_name pre_sql args } { 

	overload db_multirow with qd support

    } {
	
	set full_statement_name [db_qd_get_fullname $statement_name]
	set sql [ db_qd_replace_sql $full_statement_name $pre_sql]   
	
        set cmd [concat [list ::nstcl::db_multirow $var_name $statement_name $sql] $args]
#        puts "db_multirow executing the following : $cmd "
	uplevel 1 $cmd
   
#	puts "db_multirow is done"
}

#puts [info body db_dml]
#exit

foreach f {ns_log ns_info ns_dbsetup nsv ns_misc ns_xml ns_conn ns_cache} {
    source [file join $::pnsd::home  $f.tcl ]
}

#Use this to quickly load xql files
proc persistentArray {arrName {filename {}}} {
    upvar 1 $arrName arr
    array set arr {} ;# to make sure it exists, and is an array
    if {$filename==""} {set filename $arrName.txt}
    set filename [file join [pwd] $filename]
    if [file exists $filename] {
        set fp [open $filename]
        array set arr [read $fp]
        close $fp
    }
    uplevel 1 [list trace var $arrName wu [list persist'save $filename]]
}

proc persist'save {filename arrName el op} {
    upvar 1 $arrName arr
    switch -- $op {
        w {set value $arr($el)}
        u {set value {}}
    }
    set    fp [open $filename a]
    puts  $fp [list $el $value]
    close $fp
}




proc db_qd_load_query_file { args } {
    # no-op
}
#uncommenting this line will ensure that OACS_FULLQUERIES is loaded from a file, 
# and xql files will not be parsed on startup
set ::pnsd::invariants(db_qd_load_query_file) 1
persistentArray OACS_FULLQUERIES xql.dat




proc db_qd_log {level msg} { }
#Turn off qb_qd_log ... it's very verbose
#set ::pnsd::invariants(db_qd_log) 1

</t>
<t tx="T10">@ignore
# ns_misc.tcl - miscellaneous ns_* api procedure stubs which allow aolserver code to be executed from tclsh
# 
&lt;&lt;copyright&gt;&gt;

# 10/2002


package require uri
package require sha1
package require ncgi 
#puts [db_string current_time_from_solid { select now() }]
#

proc ns_register_filter { stage http_method match name } { 
    
    ns_log notice "ns_register_filter: registering $stage $name for $http_method all $match \n\n"
    
}
proc ns_config { section item args} {
    
    switch $item {
	"User"     { return "Administrator" }
	"Password" { return "" }
	"DataSource" { return "localhost::openacs" }
	"Driver"   { return "" }
	"Verbose"  { return "On" }
	"pgbin"    { return "c:/cygwin/bin" } 
	"DebugP"   { return "1" }
	"LogDebugP" { return "1" }
	"Hostname" { return "localhost" }
	"ClusterEnabledP" { return "false" }
	"SessionTimeout" { return 600 }
	"SessionRenew"   { return 600 }
	"NumberOfCachedSecretTokens" { return 30 }
	"PerformanceModeP" { return 0 }
	"SessionSweepInterval" { return 60 }
	"directorylisting" { return simple 
	    return fancy }
	"MaxSize" { 
	    # for memoize
	    return 200000
	} 
	"SystemOwner" { return johnseq@pobox.com }
	"ExtensionPrecedence" { return ".tcl .adp" }
	"SystemName" { return "PortableNSD" }
	"AllowPersistentLoginP" { return On }
	"PersistentLoginDefaultP" { return On }
	"RestrictErrorsToAdminsP" { return Off }
	"AutomaticErrorReportingP" { return On }
	"RefreshCache"  { return On }
	"HomeURL" { return "http://localhost/pvt" }
	"HomeName" { return "Home Base" }
    }
    
    ns_log notice "ns_config called with $section $item"

}

proc ns_configsection { path }  { 
    ns_log warning "no-op: ns_configsection called with arg $path "
    return 
}

proc ns_mutex { cmd args } { 
    return $cmd
}




#Must handle -bind flag
ad_proc ns_pg_bind { command db sql {bind "" } } { 
    #set cmd [list db_bind_var_substitution $sql $bind ] 
    #set sql [uplevel 1  $cmd ] 
#    ns_log Info "ns_pg_bind: sql =  $sql"
#First replace bound variables ... skip this - nstcl should do this for me.
#     proc quotify { var } {
# 	upvar 2 $var var2
# 	regsub -all {'} $var2 {''} var2
# 	return "'$var2'"
#     }

#     #puts [pg_bind $sql]
#     regsub -all {\[} $sql {\\[} sql
#     regsub -all {\]} $sql {\\]} sql
#     regsub -all  {\:(\w+)} $sql  { [ quotify \1 ]}  sql

#     set sql [subst $sql ] 


#    set poolname [ns_db poolname $db ]
#Then run the query

    #ignore db for now
    switch $command {
	"select" { 	
	    #no name was passed in...
	    set cmd [list ns_db select $db $sql]	    
#	    return [ns_db select $db $sql]	    
	    uplevel 1 $cmd
	    return
	    
	}
	"1row"   {	    
	    set cmd [list db_1row "ns_pg_bind_1row"  $sql -column_set setid]
	    uplevel 1 $cmd
#	    db_1row "ns_pg_bind_1row"  $sql -column_set setid
#	    return $setid
	    upvar 1 setid setid
	    return $setid
	}
	"0or1row" { 	    
	    set cmd [list db_0or1row "ns_pg_bind_0or1row" $sql -column_set setid ]
	    uplevel 1 $cmd
	    upvar 1 setid setid
	    return $setid

	}
	"dml" {

	    db_dml "ns_pg_bnd_dml" $sql
	    #return dummy results
	    set dml_setid  [ns_set create dml_results]
	    ns_set put $dml_setid result 1
	    return $dml_setid
	}

    }
    

    ns_log Error "ns_pg_bind called with cmd = $command"
}
 

proc ns_write  { text } { 
    ns_log notice "ns_write says: $text "
    
    append ::pnsd::__http_stream $text
}



#    convert querystring into ns_set
proc ns_getform {} {

    

    set form [ ns_set create cgivars ]
    foreach { pair } [ split $::pnsd::querystring &amp; ] {
	set keyval  [ split $pair = ]
	set key [lindex $keyval 0]
	set val [ncgi::decode [lindex $keyval 1]]
	ns_set put $form $key $val
	
    }



    ns_log notice "ns_getform returning\n [ns_set print $form]"    
    return $form
    
} 



proc ns_sha1 { args } { 
    return [::sha1::sha1 [lindex args 0]]
}




proc ns_schedule_proc { args } { 
    ns_log Debug "(no-op)ns_schedule_proc called with $args "

}

proc ns_returnbadrequest { args } {
    ns_log Debug "(no-op)ns_returnbadrequest called with $args "
}


proc ns_register_adptag { args } {
    ns_log Debug "ns_register_adptag called with $args "

}


proc ns_register_proc { type path procname } {
    ns_log Debug "ns_register_proc called with TYPE: $type PATH: $path PROCNAM: $procname "
}

proc ns_unregister_proc { type path args } { 
    set procname ""
    if { [info exists args] } {
	set procname [lindex $args 0]
    }
    ns_log Debug "ns_unregister_proc called with TYPE: $type PATH: $path PROCNAM: $procname  "
}

proc template::filter { args } {

    ns_log Debug "template::filter called with $args "
}


# incomplete
proc ns_eval { args } {
    ns_log notice "ns_eval (disabled) called with $args"
#    uplevel #0 $args 
}

proc ns_returnerror {status msg } {
    set ::pnsd::__http_mime $status
    set ::pnsd::__http_stream $msg

#    ns_log notice "ns_returnerror (disabled) called with $args"
       
}

if {[lsearch [namespace children] ::math] == -1} {
    package require math
#    namespace import ::math::*
}

proc ns_rand { args } { 
    if { [llength $args ] == 0 } {
	return [::math::random]
    } else {
	return [::math::random [lindex $args 0 ] ]
    }
}


#proc ad_return_template 
# {source_type source


proc ns_returnredirect { redir } { 
    ns_log Notice "ns_returnredirect to $redir"
    array set url [uri::split $redir]

    set $::pnsd::url $url(path)
    set $::pnsd::querystring  $url(query)
    rp_handler

}


#overload this..
proc ns_return { status type string } {
    ns_log notice " ns_return: status $status type=$type length=[string length $string] "    

    set ::pnsd::__http_mime $type
    set ::pnsd::__http_stream $string
    
    return 
}



proc ns_normalizepath { path } { 
    ns_log notice " ns_normalizepath called with $path "
    return $path

    #    return [file normalize $path] # nb: this works in 8.4
}


array set mimetypes [list \
     .ai application/postscript \
     .aif audio/aiff \
     .aiff audio/aiff \
     .ani application/x-navi-animation \
     .au audio/basic \
     .avi video/x-msvideo \
     .bin application/x-macbinary \
     .bmp image/bmp \
     .dp application/commonground \
     .exe application/octet-stream \
     .gif image/gif \
     .gz application/x-compressed \
     .hqx application/mac-binhex40 \
     .htm text/html \
     .html text/html \
     .jfif image/jpeg \
     .jpe image/jpeg \
     .jpg image/jpeg \
     .jpeg image/jpeg \
     .map application/x-navimap \
     .mov video/quicktime \
     .mpe video/mpeg \
     .mpeg video/mpeg \
     .mpg video/mpeg \
     .nvd application/x-navidoc \
     .nvm application/x-navimap \
     .pbm image/x-portable-bitmap \
     .pdf application/pdf \
     .pgm image/x-portable-graymap \
     .pic image/pict \
     .pict image/pict \
     .pnm image/x-portable-anymap \
     .ps application/postscript \
     .qt video/quicktime \
     .ras image/x-cmu-raster \
     .rgb image/x-rgb \
     .rtf application/rtf \
     .sit application/x-stuffit \
     .snd audio/basic \
     .stl application/x-navistyle \
     .tar appliation/x-tar \
     .text text/plain \
     .tgz application/x-compressed  \
     .tif image/tiff \
     .tiff image/tiff \
     .txt text/plain \
     .xbm image/x-xbitmap \
     .xpm image/x-xpixmap \
     .wav audio/x-wav \
     .z application/x-compressed \
     .zip application/x-compressed ]


proc ns_guesstype { filename } { 
     global mimetypes
          
     set ext [ file extension $filename ]	

     if { [info exists mimetypes($ext)] } { 
          return $mimetypes($ext)
     }

#take a guess
     return "text/html"
}

proc ns_startcontent {args  }  { 
    ns_log warning "ns_startcontent (disabled) called with $args"
    
}

proc ns_returnnotfound {} {
    ns_log warning "(no-op) ns_returnnotfound "
}

proc ns_url2file { url } {
    return [file join [ns_info pageroot] $url]
}




::nstcl::ad_proc -public template::adp_init { type file_stub } {
 
  # this will return the name of the proc if it exists
  set proc_name [info procs ::template::mtimes::${type}::$file_stub]

  set pkg_id [apm_package_id_from_key acs-templating]
  set refresh_cache [ad_parameter -package_id $pkg_id RefreshCache dummy\
			 "when needed"]

  if {[string equal $proc_name {}] || [string compare $refresh_cache "never"]} {
    set mtime [file mtime $file_stub.$type]
    if {[string equal $proc_name {}] || $mtime != [$proc_name]
	|| [string equal $refresh_cache "always"]} {

      # either the procedure does not already exist or is not up-to-date

      switch -exact $type {

	tcl {
	  set code [template::util::read_file $file_stub.tcl]
	}
	default { # works for adp and wdp

	    #jjs: added base-level argument
	  set code "[adp_compile -file $file_stub.$type] 1"
	}
      }

      # wrap the code for both types of files within an uplevel in
      # the declared procedure, so that data sources are set in the 
      # same frame as the code that outputs the template.
      set proc_name ::template::code::${type}::$file_stub

      proc $proc_name {} "
    	uplevel {
    	  $code
    	}
      "
      proc ::template::mtimes::${type}::$file_stub {} "return $mtime"
    }
  }

  ns_log debug "template::adp_init returning $proc_name "
#  puts "proc $proc_name {}  { [info body $proc_name] } "

  return $proc_name

}

proc ns_adp_parse { args }  { 

    set block1 [ subst { ::template::adp_compile $args }]

    set ccmd  [ uplevel 1  $block1 ]   
#    return "&lt;pre&gt;[    info body $ccmd ]&lt;/pre&gt;"
    
    #set block2 [ subst { ::template::adp_eval $ccmd  } ]
    ns_log debug "ns_adp_parse evaling $block1 "

    set block2 [ subst { ::template::adp_eval $ccmd -inline } ]
    return [uplevel 1 $block2 ]

#    return "$ccmd"

}


::nstcl::ad_proc -public template::adp_prepare {} {

  uplevel {

    if { [file exists $__adp_stub.tcl] } {

	# ensure that data source preparation procedure exists and is up-to-date
	template::adp_init tcl $__adp_stub

	# remember the file_stub in case the procedure changes it
	set __adp_remember_stub $__adp_stub

	# execute data source preparation procedure
	::template::code::tcl::$__adp_stub
	ns_log debug "Executed data procedure"
#	eval $data_proc

	# propagate aborting
	global request_aborted
	if [info exists request_aborted] {
	    ns_log warning "propagating abortion from $__adp_remember_stub.tcl\
          (status [lindex $request_aborted 0]): '[lindex $request_aborted 1]')"
	    template::adp_abort
	}
	
	# if the file has changed than prepare again
	if { ! [string equal $__adp_stub $__adp_remember_stub] } {
	    template::adp_prepare;			# propagate result up
	} { return 1 }
    }
    return 0
}
}

# @public adp_abort

# Terminates processing of a template and throws away all output.

proc template::adp_abort {} { error ADP_ABORT }


# This proc gets run by each compiled template... it will initialize the keys of the __page_contract_property array
proc ad_init_properties {} {

    ns_log Debug "initing properties [info level -1]"
    uplevel 1 {
	if {[info exists __page_contract_property]} {
#	    ns_log Debug "properties exist"
	    foreach name [array names __page_contract_property] { 
		ns_log Debug "setting '$name' "
		#node is used by templating...
		if {![info exists $name] &amp;&amp;  $name != "node" &amp;&amp; ![array exists $name]} { 
		    variable $name ""		    
		    #trace variable $name w monitor
		}
	    }
	    
	} else {
	    ns_log warn "ad_init_properties: __couldn't find __page_contract_property"
	}
    }
}


#Overload this command from acs-templating
::nstcl:::ad_proc adp_parse_ad_conn_file {} {
    handle a request for an adp and/or tcl file in the template system.
} {
    set __adp_stub [file root [ad_conn file]]
    namespace eval template variable parse_level ""
    ns_log Debug "YYY(adp_parse_ad_conn_file): [ad_conn file]\n [file exists [ad_conn file]] [file root [ad_conn file]] "

    ad_conn -set subsite_id [site_node_closest_ancestor_package "acs-subsite"]
    
#    set src [ad_parameter -package_id [ad_conn subsite_id] DefaultMaster dummy "/www/default-master"]
#    set ::nstcl::template::default_master [template::util::url_to_file $src [ad_conn url]]
#    set ::nstcl::template::default_master [file join [ns_info "pageroot"] default-master]

    
#  set mime_type [get_mime_type]
#  set template_extension [get_mime_template_extension $mime_type]

    #set up tcl data sources in this stack frame if a tcl file exists
    if { [catch { template::adp_prepare } errMsg] } {
	
	# return without rendering any HTML if the code aborts
	if { [string equal $errMsg ADP_ABORT] } { 
	    return "" 
	} else {
	    global errorInfo errorCode
	    error $errMsg $errorInfo $errorCode
	}
    }
    # if we get here, adp_prepare ran without throwing an error.
    # and errMsg contains its return value
    
    # initialize the ADP output
    set __adp_output ""

    variable parsed_template ""
    if { [file exists $__adp_stub.adp] } { 

	ns_log Debug puts "Parsing template ..." 	
	template::adp_init adp $__adp_stub

	#execute adp proc 
# should use this...
#	set parsed_template [ template::code::adp::$__adp_stub ]
# (jjs)initially changed to this
	set parsed_template [ ns_adp_parse -file $__adp_stub.adp ] 
# (jjs)now using this to satisfy qd
#	set parsed_template [template::adp_parse $__adp_stub]

    }
    db_release_unused_handles
    
    if {![empty_string_p $parsed_template]} {
        set mime_type [template::get_mime_type]
        set header_preamble [template::get_mime_header_preamble $mime_type]

	ns_return 200 $mime_type "$header_preamble $parsed_template"
    }

    
}

#jjs: not used
proc template::adp_parse { __adp_stub } {

	template::adp_init adp $__adp_stub
	set parsed_template [ ns_adp_parse -file $__adp_stub.adp ] 
	
	return parsed_template

}

			  
#generic script to call when tracing variables
proc monitor {name args} {
    ns_log Debug "monitor $name"
    upvar $name value
    ns_log Debug "monitor reports '[info level 1]' changes '$name' to '$value'"
}


proc ns_queryget { key } {     
    return [ ns_set value [ ns_getform ] $key ]
} 

    </t>
<t tx="T11">@doc

Messing around... I'm using this file to create the autoindex for autoloading.


@code

package require Tclx
source pnsd-init.tcl


auto_mkindex openacs-4/tcl *.tcl

exit

source c:/temp/openacs-4/packages/acs-tcl/tcl/security-procs.tcl
proc unknown {cmdName args} {
    global OACS_PROCS
    puts "$cmdName $OACS_PROCS($cmdName)"

    if [info exists $OACS_PROCS($cmdName)] {
	ns_log info "Sourcing $OACS_PROCS($cmdName)"
	
	uplevel 1 [subst {$cmdName $args}]
	return
    }

    ns_log Error "$cmdName not found!"
    error "$cmdName not found!"
}
puts $OACS_PROCS(ad_user_login)
cmdtrace on notruncate
ad_user_login 2537

#packages/acs-tcl/tcl/security-init.tcl
#packages/acs-tcl/tcl/site-nodes-init.tcl

#Inits I don't really need:
#packages/acs-tcl/tcl/utilities-init.tcl
#packages/acs-tcl/tcl/request-processor-init.tcl
#notice    {Loading packages/acs-tcl/tcl/20-memoize-init.tcl...}
#notice    {Loading packages/acs-tcl/tcl/admin-init.tcl...}
#notice    {Loading packages/acs-tcl/tcl/database-init.tcl...}
#notice    {Loading packages/acs-tcl/tcl/document-init.tcl...}
</t>
<t tx="T12">@doc

These routines will minimally load enough of OpenACS so that the remainder can be auto-loaded

@code
package require profiler
#::profiler::init 
source [file join [file dirname [info script]] pnsd-init.tcl ]

source [file join $::pnsd::home pnsd-templating.tcl ]

pnsd::lock_procs

proc db_bootstrap_checks {errors error_p} {}
set ::pnsd::invariants(db_bootstrap_checks) 1
#proc ns_log {args} {}




#puts [info body template::adp_prepare]
lappend auto_path [file join [file dirname [info script]] lib]


#jjs:stolen from 0-acs-init.tcl
set root_directory [file dirname [string trimright [ns_info tcllib] "/"]]
nsv_set acs_properties root_directory $root_directory

#jjs:stolen from bootstrap.tcl ... still need it?
nsv_set proc_source_file . ""

# Initialize ad_after_server_initialization.
nsv_set ad_after_server_initialization . ""


apm_source [file join $root_directory packages/acs-bootstrap-installer/installer-init.tcl ]
set files [glob -nocomplain "$root_directory/packages/acs-bootstrap-installer/tcl/*-procs.tcl"]
if { [llength $files] == 0 } {
    error "Unable to locate $root_directory/packages/acs-bootstrap-installer/tcl/*-procs.tcl."
}

foreach file [lsort $files] {
    ns_log Notice "Bootstrap: sourcing $file"
    apm_source $file
}



db_bootstrap_set_db_type database_problem


#check for  $database_problem
#    if { ![info exists database_problem] } {
#        set db_fn "$root_directory/packages/acs-bootstrap-installer/db-init-checks-[nsv_get ad_database_type .].tcl"
#        if { ![file isfile $db_fn] } {
#            set database_problem "\"$db_fn\" does not exist."
#        } else {
#            apm_source $db_fn
#        }
#        db_bootstrap_checks database_problem error_p
#    }


proc populate_secret_tokens_cache {} {}
set pnsd::invariants(populate_secret_tokens_cache) 1

#Move this into a watched variable?
db_1row get_one_token " select t.token_id, t.token from (select token_id, token,  random()
   from secret_tokens order by 3) t  limit 1"
ns_cache set secret_tokens $token_id $token 



apm_bootstrap_load_libraries -init acs-tcl
#apm_bootstrap_load_libraries -init -procs acs-subsite
#apm_bootstrap_load_libraries -init acs-subsite
#apm_bootstrap_load_libraries -init acs-templating
#apm_bootstrap_load_libraries -init -procs acs-developer-support

#If we're retrieving a tcl or adp file
# from acs-integration-init ...
rp_register_extension_handler adp adp_parse_ad_conn_file
rp_register_extension_handler tcl adp_parse_ad_conn_file
apm_source $root_directory/packages/acs-templating/tcl/0-procs.tcl


#apm_source [file join $root_directory packages/acs-tcl/tcl/00-database-procs-postgresql.tcl]


#set files [glob -nocomplain "$root_directory/packages/acs-tcl/tcl/*-init.tcl"]
#foreach file [lsort $files] {
#    ns_log Notice "Bootstrap: sourcing $file"
#    apm_source $file
#}



#apm_source [file join $root_directory packages/acs-tcl/tcl/request-processor-init.tcl]
#apm_source [file join $root_directory packages/acs-tcl/tcl/request-processor-procs.tcl]

#need this for ad_page_contract
#apm_source [file join $root_directory packages/acs-tcl/tcl/tcl-documentation-procs.tcl]


#initialize site nodes
#apm_source [file join $root_directory packages/acs-tcl/tcl/utilities-procs.tcl]

#I don't want to initialize the cache each time...
#proc populate_secret_tokens_cache { args } {}
#set ::pnsd::invariants(populate_secret_tokens_cache) 1
#apm_source [file join $root_directory packages/acs-tcl/tcl/security-init.tcl]

#apm_source [file join $root_directory packages/acs-tcl/tcl/site-nodes-init.tcl]



</t>
<t tx="T13"># ns_conn  defines all connection/request parameters.
# Since this is the defaul implementation of ns_conn,  we'll just 
# supply dummy values that will make sense for the installer
# Actual run-time environments like fastCGI or mod_dtcl should redefine
# this command with their own version
# TODO It should be coded to be able to run from a file of stored values for debugging

&lt;&lt;copyright&gt;&gt;


proc ns_conn { cmd args } {

    switch $cmd {
	authpassword { 
	    ns_log Warn "ns_conn authpassword not implemented yet"
	    return ""
	}
 	authuser { 
	    ns_log Warn "ns_conn authuser not implemented yet"
	    return ""
	}
 	close { 
	    ns_log Warn "ns_conn close not implemented yet"
	    return }
 	contentlength { ns_log Warn "ns_conn contentlength not implemented yet"
	    return }
 	driver { ns_log Warn "ns_conn driver not implemented yet"
	    return }
 	form { ns_log Warn "ns_conn form not implemented yet"
	    return }
 	headers { #ns_log Warn "ns_conn headers not fully implemented yet"
	    return $::pnsd::headers_id 
	}
 	host { #ns_log Warn "ns_conn host not implemented yet"
	    return [info hostname] }
 	isconnected { 
	    #ns_log Warn "ns_conn isconnected not implemented yet"
	    return 1}
 	location { 
	    return "http://[ns_conn host]:[ns_conn port]/" }
 	method { 
#	    ns_log Warn "ns_conn method not fully implemented yet"
	    return "GET"
	}

 	outputheaders { #ns_log Warn "ns_conn outputheaders not implemented yet"
	    return $::pnsd::output_headers_id
	}
 	peeraddr { 
	    #ns_log Warn "ns_conn peeraddr not implemented yet"
	    return "127.0.0.1"
	}
 	port { 
#	    ns_log Warn "ns_conn port not implemented yet"
	    return "80"
	}
 	protocol { 
	    return "http"}
 	query { 
#	    ns_log Warn "ns_conn query not implemented yet"

	    return $::pnsd::querystring
	}
 	request { 
	    return "GET [ns_conn url] HTTP/[ns_conn version]" 
	}
 	url { 
#	    ns_log Warn "ns_conn url not fully implemented yet"
	    return $::pnsd::url
	}
 	urlv { 
#	    ns_log Warn "ns_conn urlc not implemented yet"
	    set urllist [split [ns_conn url] /]
	    return [lrange $urllist 1 [llength $urllist]]
	}
 	urlc { 
# 	    ns_log Warn "ns_conn urlv not implemented yet"
	    set urlv [expr [llength [split [ns_conn url] /]] - 1]
	    if {$urlv == ""} { return 0 }
	    return $urlv
	}
 	version { 
#	    ns_log Warn "ns_conn version not implemented yet"
	    return "1.0"
	}
    }

}
</t>
<t tx="T14"></t>
<t tx="T15">
OpenACS would have many more supporters if code developed for it would run on 99-100% of web servers instead of &lt;&lt; %1. 
CGI is not evil - it works fine for lightly used applications, especially for intranet.
</t>
<t tx="T16">CGI?  Are you kidding me?

Okay,  CGI's not the highest performing web API in the world,  but combined with modern hardware it works great for the kind of low traffic intranet applications that OpenACS excels at.

The idea is the following:  instead of going through a lengthy startup/initialization process each time we launch an openacs-enabled interpreter,  we'll source the bare minimum files,  and auto-load the remaining commands as they are called.  To do this,  we simply create a tclIndex file similar to how makepath

The theory is that each OpenACS page might use a very tiny fraction of the commands in the framework,  and that enduring the CGI overhead and foregoing the extensive caching _might_ be acceptable.

Right now the load process includes some CPU intensive cache-loading code that really only makes sense in a persistent interpreter.  This code needs to be overloaded to make the code perform fast enough to serve web pages.  

In it's current state,  the auto-loading implementation is not quite ready to serve web pages,  but it makes debugging code *much* faster.
</t>
<t tx="T17">portable.nsd -&gt; openacs portability framework
Copyright (C) 2002 John Sequeira,  56 Maple Ave #2 Cambridge MA 02139,  johnseq@pobox.com   

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

Portions of this distribution are copyright Tom Pointdexter,  Michael Cleverly.</t>
<t tx="T18">TCLHTTPD/portable.nsd INSTALLATION INSTRUCTIONS

1) Install tclhttpd

http://sourceforge.net/projects/tclhttpd/

2) The file 'tclhttpd_openacs.tcl' must be placed in the [tclhttpd root]/custom/ folder.  This will route all tclhttpd requests through the request processor [rp_handler].

3) There is a line in this file that sources [portable.nsd root]/tclhttpd.tcl.  For now,  hardcode the location of [portable.nsd root].

From there you can run tclhttpd by doing:

 cd [tclhttpd root]
 tclsh ./bin./httpd.tcl -debug 1




* ERRATA

11/2002 There was a bug in tclhttpd that interfered with loading of custom libraries.  You may need to get the source from CVS if this hasn't been fixed in 3.4.2.



</t>
<t tx="T19">Portable.NSD  ("pnsd")

*README 

This is portable.nsd,  a set of tcl commands that should make it easier to run OpenACS code in other environments. 

The initial goal for portable.nsd was to allow arbitrary OpenACS to run outside AOLServer.  This turned out to be a bit too ambitious for a solo effort.  There is a decent amount of legacy code in OpenACS -&gt; calls to nsv,  ns_share etc. that is not supported by pnsd.  Much of that has been implemented in pnsd in a straightforward, albeit single-threaded way,  but I keep finding arguments and parameters that my code doesn't cover.

So while the pnsd code won't let the 70,000 lines of TCL run unaltered on different web platforms,  I will let you run greater than 99%.  This means that if you are creating a new application to run on OpenACS,  you can stay away from the 1% that causes headaches and (soon) have multiple platform and webserver support for your application.  If you are porting an old OpenACS application to run in a different environment,  your porting effort will likely be quite brief.

Portable.NSD consists of :
1) a native TCL implementation of much of AOLServer's TCL API,  which map some AOLServer functions to it's appropriate analog in FastCGI/tclhttpd/tclsh/etc.  For this I'm relying heavily on Michael Cleverly's nstcl, and in the future probably on AOLServer's unbundled DLL's.
2) a set of redefined or overloaded OpenACS TCL commands which remove their platform dependencies and make them consistent with nstcl. 

For fun,  it includes:
An experimental FastCGI implementation (tested with Apache)
An experimental tclhttpd implementation 
A tclsh version of /acs-bootstrap-installer/installer/auto-install.tcl,  that can populate your database with a working OpenACS instance.
A more package-specific installer,  which can install and enable arbitrary package(s) on top of a base install
A cached startup - XQL files are preparsed and catched into TCL, OpenACS commands are auto-loaded on demand by a custom tclIndex implementation.  You don't have to load N thousand files to run a three line script.
A simple unit testing framework,  based on tcltest, allowing you to invoke page code without going through the web server layer.  I've written tests for some of the bugs that exist in pnsd,  so that you will either know code to avoid,  or help fix some of them.

I should note that all of the above can be run via tclsh or one of the tcl debuggers (I test with TclPro 1.2).

There are definitely bugs to fix,  but I can confidently say that more things are working than not.  While that's not the most ambitious development milestone,  my hope is that it's turned to corner on becoming more widely useful to the OpenACS developer community.   

I would like to make robust FastCGI deployment a reality,  because that will be the best short-term alternative to running AOLServer.


John Sequeira
johnseq@pobox.com
http://www.pobox.com/~johnseq/projects/portable.nsd
</t>
<t tx="T20">* portable.nsd TODO

	
HIGH
 -Make FastCGI AppClass connections work. (Difficult/impossible without compiled extension)

MEDIUM 
 -make ad_table nstcl compatible
 -write tests for broken functionality
 -fix query dispatcher for cgi use 
 
LOW
 -config file parsing
 -better solution for ns_schedule
 -revisit thread safety
 -IIS/tcom prototype


</t>
<t tx="T21">
RELEASE NOTES for portable.nsd 0.7b

3/5/2002
	
tclsh support for headless OpenACS code is quite good.  This means that writing tclsh installers,  running OpenACS scripts as cron jobs,  debugging them with TclPro, running inside Postgresql using pg/tcl etc. should work fine.  The most problematic routines revolve around HTML generation,  because they often interact with the template engine in ways not directly supported by nstcl.  Also, that seems to be where the most churn has occurred in OpenACS long history.

tclhttpd support is experimental.  It has not been extensively tested, and since it doesn't handle garbage collection.
 
FastCGI support for AppClass connections is broken due to a Tcl feature that has since gone unmaintained.  FastCGI External Server (TCP/IP) connections work well.  In practice that means that you can have a single Tcl interpreter sitting behind a single web server.  To make this perform acceptably you would want to use some sort of round robin proxy or load balancing.   AppClass connections can be set to recycle after a certain number of hits,  which means they would work well even without robust garbage collection.

All portable.nsd code assumes it's running in a single-threaded interpreter.  FastCGI is designed to handle this scenario,  but it puts serious constraints on implementing OpenACS+pnsd in native/threaded web server API's like Apache's mod_tcl and IIS's Active Server Pages.  Much of the AOLServer support for thread-safe globals has been implemented as simple globals,  which would choke mod_tcl and ASP.  

Cache synchronization has been implemented by simply reading and writing cached values to the database.  This obviously doesn't really give you the speed increase of an in-memory cache,  but I believe between tequila or Michael C's shared nsset's this problem is solvable.

ns_xml has been replaced by tDom calls.

LIMITATIONS

* portable.nsd only tested with postgresql 7.2.3  and OpenACS 4.5 at this time.  The majority of the framework is more concerned with web connectivity,  and is database agnostic.  Oracle support should require a day or two of tweaking,  but it has not been worked on at all.  I don't expect 4.6 support to be too painful,  but haven't tried it.

* nsd.tcl config file parsing is not yet working.  You must edit the config file values directly in ns_misc.tcl  (see the ns_config and ns_info commands defined in that file).

* right now only the tclhttpd implementation supports serving non-HTML files.  

* Memoization and ns_cache have not been tested yet in a cluster.  ns_cache has been implemented using a database lookup table.  

* ns_register_proc, ns_register_filter are not supported

* no load testing has been performed.


John Sequeira
johnseq@pobox.com
1/2003
</t>
<t tx="T22">portable.nsd Installation Steps

PREREQUISITES:

You'll need tcl,  PostgreSQL,  Michael Cleverly's nstcl (currently included), tDom,  and the tcl postgresql library libpgtcl.so if you'll be working with postgres ( available from http://ftp.postgresql.org ).  FastCGI and tclhttpd support have other dependencies (see their respective install files).

SETUP:

1) Config file

Config file parsing doesn't yet work in this release. You must edit the config file values directly in ns_dbsetup.tcl, portable.nsd-init.tcl and ns_misc.tcl.  Specifically,  see the ns_config and ns_info commands defined in ns_misc.tcl for more detail on what you need to define.


2) Database

I'm assuming you've already got postgresql installed.
Make sure you have the plpgsql language installed in the database:  "createlang -d &lt;database_name&gt; plpgsql"
Execute "createdb &lt;database_name&gt;".  The pnsd code has a hardcoded default of "openacs" on localhost.

ns_dbsetup.tcl contains the hardcoded connection string values.

3) Packages

To run the OpenACS installer,  type "tclsh install.tcl".  This loads all the openacs tcl libraries and runs the auto-install code.  The end result should be a working openacs postgres database.  You can skip this step if you're working with an existing OpenACS installation.


John Sequeira
johnseq@pobox.com
October 2002
</t>
<t tx="T23">INSTALL.FCGI


Using FastCGI to run OpenACS code should be a good compromise between high performance and portability.  Unfortunately,  FastCGI support in TCL is not actively maintained,  and so currently only works as an FastCgiExternalServer.  This means you must pre-spawn an interpreter which listens for incoming FastCGI requests, relayed to it from the web server.  This isn't too much different than running AOLServer behind a proxy server.  However, since portable.nsd isn't really threaded,  and doesn't have garbage collection,  spawning a single interpreter can't be considered more than an alpha-level implementation.  You could load balance your interpreters,  but the setup starts getting complicated and would not be for the faint of heart.  You'd really have to think about why running AOLServer as a proxy behind your main server wouldn't be a better idea.

Note that implementing OpenACS as an AppClass style server would require Tom Poindexter's Fcgi.tcl to be updated to work with recent interpreters.  AppClass-style servers are dynamically spawned and launched by the web server,  and are also garbage collected when not needed.   This behavior would address both problems and should enable production-level FastCGI deployment.



* FastCGI Prerequites:

 - cgi.tcl     ( http://expect.nist.gov/cgi.tcl/ ) 
 - A FastCGI web server (Apache/mod_fcgi,  Zeus,  etc.)

* Overview - Apache/mod_fcgi

Here's a good overview of the necessary Apache configuration:

http://members.optushome.com.au/projectgoanna/installation.htm

</t>
<t tx="T24">I wanted to update everyone on the status of portable.nsd.  I'm happy to report that I've reached the point with the code where more things work than don't.  It's quite useful for running arbitrary OpenACS code outside of AOLServer and should be useful for automating installation and testing.  As an example,  A script to install OpenACS looks like this:
&lt;p&gt;

&lt;pre&gt;
source pnsd-init.tcl;  # read config file
pnsd::source_openacs
apm_source "$::pnsd::root/packages/acs-bootstrap-installer/installer.tcl"
apm_source "$::pnsd::root/packages/acs-bootstrap-installer/installer/auto-install.tcl"
&lt;/pre&gt;

I believe the biggest barriers to serving up pages via tclhttpd or Apache are currently thread safety and garbage collection.  These problems should go away when pnsd can replace the dependencies on global arrays and nssets by a slightly more stateless mechanisms (autoload'ing commands and caching nsset's in the database).  If someone with C coding skills were motivated to see this happen sooner,  I could use some help updating Tom Poindexter's C-based FastCGI extension to work the latest tcl and fastcgi libraries.  I'm reluctant to go this route because it requires a compiled extension,  but it would be very slick and should have performance in the ballpark of AOLServer.
&lt;p&gt;
So,  to sum up,   my original goal was to allow OpenACS to run easily in other web servers.  After working on it a bit,  I'm pursuing the slightly less ambitious goal of enabling OpenACS code to be easily ported to other web servers.  More specfically,  if an OpenACS package author want to write code which ran outside of AOLServer,  I hope in the future to define a set of guidelines to make the porting process a trivial one.
&lt;p&gt;
Feel free to &lt;a href="http://amped.dnsalias.com:8000/portable.nsd/portable.nsd.tgz"&gt;download&lt;/a&gt; the latest code and let me know what you think.

</t>
<t tx="T25"></t>
<t tx="T26">w/Openacs 5.0b.a

I Know Ive solved this problem before... I think the variable substitution is happening twice.

Whatever.



ERROR:  pg_atoi: error in "NODE_ID": can't parse "NODE_ID"

    while executing
"error $error_msg"
    (procedure "::nstcl::db_0or1row" line 29)
    invoked from within
"::nstcl::db_0or1row ns_pg_bind_0or1row {

	select site_node__new('NODE_ID','PARENT_ID','NAME','OBJECT_ID','DIRECTORY_P','PATTERN_P','CREATION_USER','C..."
    ("uplevel" body line 1)
    invoked from within
"uplevel 1 [concat [list ::nstcl::db_0or1row $statement_name $sql] $args]"
    (procedure "db_0or1row" line 20)
    invoked from within
"db_0or1row ns_pg_bind_0or1row {

	select site_node__new('NODE_ID','PARENT_ID','NAME','OBJECT_ID','DIRECTORY_P','PATTERN_P','CREATION_USER','CREATION_I..."
    invoked from within
"if 1 {db_0or1row ns_pg_bind_0or1row {

	select site_node__new('NODE_ID','PARENT_ID','NAME','OBJECT_ID','DIRECTORY_P','PATTERN_P','CREATION_USER','CREA..."
    ("uplevel" body line 1)
    invoked from within
"uplevel 1 [list if 1 $cmd]"
    ("0or1row" arm line 3)
    invoked from within
"switch $command {
	"select" { 	
	    #no name was passed in...
	    set cmd [list ns_db select $db $sql]	    
#	    return [ns_db select $db $sql]	   ..."
    (procedure "ns_pg_bind" line 26)
    invoked from within
"ns_pg_bind 0or1row {} {

	select site_node__new('NODE_ID','PARENT_ID','NAME','OBJECT_ID','DIRECTORY_P','PATTERN_P','CREATION_USER','CREATION_IP')

   ..."
    ("uplevel" body line 1)
    invoked from within
"uplevel $ulevel [list ns_pg_bind $type $db $sql"
    invoked from within
"db_exec 0or1row $db $full_statement_name $sql"
    (procedure "db_exec_plsql" line 23)
    invoked from within
"db_exec_plsql create_object "
    BEGIN
      :1 := ${package_name}.new([plsql_utility::generate_attribute_parameter_call  -prepend ":"  -indent [expr..."
    (procedure "package_instantiate_object" line 106)
    invoked from within
"package_instantiate_object -extra_vars $extra_vars site_node"
    (procedure "site_node::new" line 9)
    invoked from within
"site_node::new -name $node_name -parent_id $parent_node_id"
    (procedure "site_node::instantiate_and_mount" line 18)
    invoked from within
"site_node::instantiate_and_mount -package_key acs-lang"
    (procedure "apm_mount_core_packages" line 6)
    invoked from within
"apm_mount_core_packages"
    (procedure "install_do_packages_install" line 48)
    invoked from within
"install_do_packages_install"
    (file "c:/web/openacs/packages/acs-bootstrap-installer/installer/install.tcl" line 68)
    invoked from within
"source $path"
    invoked from within
"if {1} {
set ::pnsd::url /installer/install


set ::pnsd::querystring "email=johnseq%40pobox.com&amp;username=system&amp;first_names=John&amp;last_name=Sequeira&amp;p..."
    (file "bin/install-openacs.tcl" line 27)
NOTICE:  Adding missing FROM-clause entry for table "acs_object_id_seq"</t>
</tnodes>
</leo_file>
