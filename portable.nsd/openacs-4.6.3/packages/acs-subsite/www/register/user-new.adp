<master>
  <property name="title">Register</property>
  <property name="focus">usernew.password_confirmation</property>



Register as a user of <a href="index">@system_name@</a>

<form method=post action="user-new-2" name="usernew">
@export_vars@

<if @no_require_password_p@ eq 0>

  <h3>Security</h3>

  We need a password from you to protect your identity as you
  contribute to the Q&A, discussion forums, and other community
  activities on this site.

  <p>
  <table>
  <tr>
    <td>Password:</td>
    <td><input type="password" name="password" value="@password@" size="10" /></td>
  </tr>
  <tr>
    <td>Password Confirmation:</td>
    <td><input type="password" name="password_confirmation" size="10" /></td>
  </tr>
  </table>
  <p>

  Leading or trailing spaces will be removed by the server.  
  Don't obsess too much over your choice of password; if you forget it, our server will
  offer to email it to you.

</if>
<else>
  <h3>Security</h3>

  We will generate and send to you a random password when your registration
  is confirmed. Once you login, you can change your password.
  <input type="hidden" name="password" value="somevalue" />
  <input type="hidden" name="password_confirmation" value="othervalue" />
</else>

<h3>About You</h3>

We know your email address already: "@email@".  But we need your full
name to generate certain kinds of user interface.

<p>

Full Name:    <input type="text" name="first_names" size="20" value="@first_names@" /> <input type="text" name="last_name" size="25" value="@last_name@" />
<p>

<if @require_question_p@ true and @custom_question_p@ true>

  We also need a customized question and answer to reset your password if you forget.

  <p>
  
  Question: <input type="text" name="question" size="30" /><br>
  Answer: <input type="text" name="answer" size="30" />

  <p>

</if>

If you have a Web site, we'll be able to point searchers there.

<p>

Personal Home Page URL:  <input type="text" name="url" size="50" value="http://">

<p>

<center>
<input type="submit" value="Register">
</center>
</form>

