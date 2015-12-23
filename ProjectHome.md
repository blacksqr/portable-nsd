<a href='http://openacs.org'>OpenACS</a> is a robust web development framework, and it has been used
for almost 13 years as a mature technology for the development of
social networks and web applications. It's in production in many
large-scale environments, such as government agencies and
organizations, universities, NGO's and companies around the world.

However, its design was completely based on AOLServer, which was at
the time the best Web and Application Server available.  While its
performance is still among the best in the market, this dependency is
becoming a problem, because most production environments rely on more
mainstream technology standards.  Getting the platform approved by
System Administrators almost always represents a barrier to adoption.

To address this,  portable-nsd was created to provide an abstraction
layer between the web server and the application framework that can
provide more deployment flexibility technology options similar to ruby
rack, perl plack, etc..
