INSTALL.FCGI


Using FastCGI to run OpenACS code should be a good compromise between high performance and portability.  


* FastCGI Prerequites:

 - cgi.tcl     ( http://expect.nist.gov/cgi.tcl/ ) 
 - A FastCGI web server (Apache/mod_fcgi,  Zeus,  etc.)
 - On a Unix platform,  you need TclX
 - On a Windows OS,  you'll need a compiled version of fcgi.tcl  ( http://lasagne.unix-ag.uni-kl.de/~bauer/fastcgi-tcl.html )


* Overview - Apache/mod_fcgi

Here's a good overview of the necessary Apache configuration:

http://members.optushome.com.au/projectgoanna/installation.htm

In high level terms,  you should define the included script pnsd-fastcgi.tcl as an AppClass server.  This means that the fastcgi process manager will prelaunch several of these scripts in anticipation of handling simultaneous requests.

use mod_rewrite to pass all URLs to this AppClass instance.

(TODO: add example httpd.conf file )

