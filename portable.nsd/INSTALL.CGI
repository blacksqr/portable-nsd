
* THIS IS EXPERIMENTAL - CGI functionality has not been implemented. 


CGI?  Are you kidding me?

Okay,  CGI's not the highest performing web API in the world,  but combined with modern hardware it works great for the kind of low traffic intranet applications that OpenACS excels at.

The idea is the following:  instead of going through a lengthy startup/initialization process each time we launch an openacs-enabled interpreter,  we'll source the bare minimum files,  and auto-load the remaining commands as they are called.  To do this,  we simply create a tclIndex file similar to how makepath

The theory is that each OpenACS page might use a very tiny fraction of the commands in the framework,  and that enduring the CGI overhead and foregoing the extensive caching _might_ be acceptable.

Right now the load process includes some CPU intensive cache-loading code that really only makes sense in a persistent interpreter.  This code needs to be overloaded to make the code perform fast enough to serve web pages.  

In it's current state,  the auto-loading implementation is not quite ready to serve web pages,  but it makes debugging code *much* faster.
