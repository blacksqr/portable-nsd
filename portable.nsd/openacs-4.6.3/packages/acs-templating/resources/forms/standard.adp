
<!-- Dark blue frame -->
<table bgcolor=#6699CC cellspacing=0 cellpadding=4 border=0>
<tr><td>

<!-- Light blue pad -->
<table bgcolor=#99CCFF cellspacing=0 cellpadding=6 border=0 width="100%">
<tr><td>

<!-- Form elements -->
<table bgcolor=#99CCFF cellspacing=0 cellpadding=2 border=0 width="100%">

  <multiple name=elements>

    <if @elements.section@ not nil>
      <tr><td colspan=2 bgcolor=#eeeeee><b>@elements.section@</b></td></tr>
    </if>

    <group column="section">

    <if @elements.widget@ eq "hidden"> 
        <noparse><formwidget id=@elements.id@></noparse>
    </if>

    <else>
      <if @elements.widget@ eq "submit">
        <tr><td align=center colspan=2>
          <noparse><formwidget id=@elements.id@></noparse>
        </td></tr>
      </if>
      <else>
       <tr>
        <if @elements.label@ not nil>
	<td><b>@elements.label@</b>&nbsp;&nbsp;
          <if @elements.help_text@ not nil>
            <br>&nbsp;&nbsp;
            <font size=-1><noparse><formhelp id=@elements.id@></noparse></font><br>
          </if>
	  </td>
        </if>
	<if @elements.widget@ in radio checkbox>
            <if @elements.label@ nil><td colspan=2>></if>
	    <else><td></else>
	    <noparse>
            <table cellpadding=4 cellspacing=0 border=0>
	      <formgroup id=@elements.id@>
		<tr><td>\@formgroup.widget@</td>
                    <td><label for="@elements.form_id@:elements:@elements.id@:\@formgroup.option@">\@formgroup.label@</label></td></tr>
	      </formgroup>
	      </table>
	      <formerror id=@elements.id@><br>
                <font color="red"><b>\@formerror.@elements.id@\@</b></font>
              </formerror>
            </noparse>
	    </td>	    
	</if>
	<else> 
	    <if @elements.widget@ eq inform>
	      <td bgcolor=#EEEEEE>
		<noparse><formwidget id=@elements.id@></noparse>
	      </td>
	    </if>
	    <else>
              <if @elements.label@ nil><td nowrap colspan=2></if>
                <else><td nowrap></else>
		<noparse><formwidget id=@elements.id@>
		<formerror id=@elements.id@><br><font 
		   color="red"><b>\@formerror.@elements.id@\@<b></font>
                </formerror></noparse>
	      </td>
	    </else>
	</else>
       </tr>
      </else>
    </else>

    </group>

  </multiple>

  </table>

</td></tr>

<if @buttons:rowcount@ gt 0>
  <tr>
    <td align="center">
      <multiple name="buttons">
        <input type="submit" name="@buttons.name@" value="@buttons.label@">
      </multiple>
    </td>
  </tr>
</if>

<!-- End of light blue pad -->
</table>

<!-- Dark blue frame -->
</td></tr>
</table>
