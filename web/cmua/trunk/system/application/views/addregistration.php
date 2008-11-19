<?php include_once('head.php');

function my_input($data)
{
    $CI =& get_instance();
    $val = $CI->db_session->flashdata($data['name']);
    //print_r($CI->db_session->userdata());
    if ($val) $data['value'] = $val;
    return form_input($data);
}

function my_radio($data)
{
    $CI =& get_instance();
    $val = $CI->db_session->flashdata($data['name']);
    if ($val == $data['value']) $data['checked'] = TRUE;
    elseif ($val !== FALSE)     $data['checked'] = FALSE;
    return form_radio($data);
}

function my_checkbox($data)
{
    $CI =& get_instance();
    $val = $CI->db_session->flashdata($data['name']);
    if ($val == $data['value']) $data['checked'] = TRUE;
    elseif ($val !== FALSE)     $data['checked'] = FALSE;
    return form_checkbox($data);
}

function my_textarea($data)
{
    $CI =& get_instance();
    $val = $CI->db_session->flashdata($data['name']);
    //print_r($CI->db_session->userdata());
    if ($val) $data['value'] = $val;
    return form_textarea($data);
}

?>
<div id="wrap">
    <div id="registration">
    <p>
The Central Maryland Ultimate Association (CMUA) will be offering two 
separate leagues this summer.  Both will be co-ed and both will be open to all 
skill levels.  Participate in one or both leagues for the same price ($20.00).<br>
<h4>The Monday Night League - Our classic Co-ed summer league</h4>
<ul><li>Who:
    <ul><li>Adult Ultimate players of all skill levels and genders are invited to play.  There is limited space for both men and women in this league, so be sure to sign up early!
    <li>Underage players may sign up only if they have a sponsor in the league.  See the “Under 18?” paragraph below for details. </ul>
<li>What:
    <ul><li>
    <li> A 16 team mixed format league.
    <li> Teams are formed via a draft of all the registered players by volunteer captains.
    <li> The mixed format for this league is 5/2 or 4/3 - offense chooses.
    <li> Two rounds are played every Monday night; plus each team will play one triple header during the regular season.
    </ul>
<li>Where:
    <ul><li> Regular season games are played at fields in the Catonsville area. 
    <li> Playoff games are also played in Catonsville, but some games will be played elsewhere (typically in Lansdowne and Columbia) in order to provide lighted fields for all games.</ul>
<li>When:
    <ul> 
    <li> Registration is held during the month of May.  The Deadline is May 24, but this league always fills up prior to the deadline!
    <li> Games are played from June to mid-August. 
    <li> First round games are scheduled to start at 6:30 PM and the last round typically ends around 9 PM.  Longer rounds are played on fields with lights; hence plan on playing till 10 PM on those evenings.
    </ul>
</ul>

<h4>The Wednesday Night League - AKA The Clique &amp; Pick League.</h4>  
<ul>
<li> Who:
    <ul>
    <li> Adult Ultimate players of all skill levels and genders are invited to play.
        <ul>
        <li> You may sign up as an individual and be drafted onto a team or
        <li> You may sign up as a member of a clique by listing the clique name in the “Exceptions” box of the registration form.
        <li> Underage players may sign up only if they have a sponsor in the league.  See the “Under 18?” paragraph below for details.
        </ul>
    </ul>
<li> What:
    <ul>
    <li> An 8 to 10 team modified-mixed format league.
    <li> Teams are formed via a two part process:
        <ul>
        <li> A clique of up to 5 players is formed prior to the draft.  (Someone from the clique must notify the league commissioner of their intent to create a team by the sign up deadline of May 24.)
        <li> A leader from the clique attends the draft and picks the rest of the team.
        </ul>
    <li> The mixed format for this league is determined at game time each week and is based on the number of women in attendance that evening.
    <li> One lengthy game is played each week, but an occasional double header may be required in order to make up a rained out game.
    </ul>
<li> Where: 
    <ul>
    <li> All games are played at fields in the Catonsville area. 
    </ul>
<li> When: 
    <ul>
    <li> Registration is held during the month of May.  The Deadline is May 24.
    <li> Games are played from June to mid-August. 
    <li> Games are scheduled to start at 6:30 PM and typically take two hours to play.
    </ul>
</ul>

<strong>To register</strong>: simply fill out this form and click "submit registration".
The next page will tell you how to pay either online or by check.<br><br>
<strong>Under 18?</strong> The summer leagues offered by the CMUA are geared towards the adult
player. Players under the age of 18 (as of June 1) may register for a Summer
League only if they have an adult sponsor within that league. It is preferred
that the sponsor be an immediate family member, however a family friend is
acceptable. An adult player that the underage player just met at a pick-up
game is not an appropriate sponsor. When registering, the underage player must
list his/her sponsor on the "Optional Chum Declaration" line.  Additionally,
he/she may not register online, since we require the parent's signature on the
<a href="/static/2008Summer_Registration_Form.pdf">registration form.</a><br><br>
Starred fields are required. Questions?  Contact us by <a 
href="mailto:cmua.summer08@comcast.net">email</a> or call Barry at 301-490-1860.<br>
</p>

<font color="red"><?php if (strlen($e) > 0) print $e; ?></font><br><br>

<?php echo  form_open('register/add'); ?>
<?php $def_size = '40'; ?>
<table id="addregistration">
<tr><td>
    Name:
    </td><td>*</td>
    </td><td>
        <?php echo  my_input(array('name'       => 'name',
                                    'id'        => 'name',
                                    'maxlength' => '1024',
                                    'size'      => $def_size,
                       ));
        ?>
    </td></tr>
    <tr><td>
    Address Line 1:
    </td><td>*</td>
    </td><td>
        <?php echo  my_input(array('name'        => 'address',
                                    'id'         => 'address',
                                    'maxlength'  => '1024',
                                    'size'       => $def_size,
                       ));
        ?>
    </td></tr>
    <tr><td>
    Address Line 2:
    </td><td> </td>
    </td><td>
        <?php echo  my_input(array('name'        => 'address2',
                                    'id'         => 'address2',
                                    'maxlength'  => '1024',
                                    'size'       => $def_size,
                       ));
        ?>
    </td></tr>
    <tr><td>
    City:
    </td><td>*</td>
    </td><td>
        <?php echo  my_input(array('name'      => 'city',
                                    'id'        => 'city',
                                    'maxlength' => '255',
                                    'size'      => $def_size,
                       ));
        ?>
    </td></tr>
    <tr><td>
    State:
    </td><td>*</td>
    </td><td>
        <?php echo  my_input(array('name'       => 'state',
                                    'id'        => 'state',
                                    'maxlength' => '2',
                                    'size'      => '2',
                       ));
        ?>
    </td></tr>
    <tr><td>
    Zip Code:
    </td><td>*</td>
    </td><td>
        <?php echo  my_input(array('name'       => 'zip',
                                    'id'        => 'zip',
                                    'maxlength' => '10',
                                    'size'      => 5,
                       ));
        ?>
    </td></tr>
    <tr><td>
    Home phone:
    </td><td> </td>
    </td><td>
        <?php echo  my_input(array('name'       => 'phone1',
                                    'id'        => 'phone1',
                                    'maxlength' => '1024',
                                    'size'      => $def_size,
                       ));
        ?>
    </td></tr>
    <tr><td>
    Mobile Phone:
    </td><td> </td>
    </td><td>
        <?php echo  my_input(array('name'       => 'phone3',
                                    'id'        => 'phone3',
                                    'maxlength' => '1024',
                                    'size'      => $def_size,
                       ));
        ?>
    </td></tr>
    <tr><td>
    Work Phone:
    </td><td> </td>
    </td><td>
        <?php echo  my_input(array('name'       => 'phone2',
                                    'id'        => 'phone2',
                                    'maxlength' => '1024',
                                    'size'      => $def_size,
                       ));
        ?>
    </td></tr>
    <tr><td>
    Date of Birth (mm/dd/yy)<sup><a href="#adob">&#x2020;</a></sup>:
    </td><td>*</td>
    </td><td>
        <?php echo  my_input(array('name'      => 'dob',
                                   'id'        => 'dob',
                                   'maxlength' => '1024',
                                   'size'      => $def_size,
                       ));
        ?>
    </td></tr>
    <tr><td>
    Gender (M/F):
    </td><td>*</td>
    </td><td>
        <?php echo  my_radio(array('name'      => 'gender',
                                   'id'        => 'gender',
                                   'value'     => 'M',
                                   'checked'   => TRUE,
                       ));
        ?> Male<br>
        <?php echo  my_radio(array('name'      => 'gender',
                                   'id'        => 'gender',
                                   'value'     => 'F',
                                   'checked'   => FALSE,
                       ));
        ?> Female<br>
    </td></tr>
    <tr><td>
    Email Address:
    </td><td> </td>
    </td><td>
        <?php echo  my_input(array('name'      => 'mailing_address',
                             'id'        => 'mailing_address',
                             'maxlength' => '1024',
                             'size'      => $def_size,
                       ));
        ?>
    </td></tr>
    <tr><td>
    Categorize Yourself:
    </td><td>*</td>
    </td><td>
        <table><tr><td>
        <?php echo my_radio(array('name'  => 'category',
                             'id'    => 'category',
                             'value' => 1,
                       ), TRUE, "checked");?>
        </td><td>Novice Player:</td></tr>
        <tr><td>
        <?php echo my_radio(array('name'  => 'category',
                             'id'    => 'category',
                             'checked' => FALSE,
                             'value' => 2,
                       ));
        ?></td><td>Pickup/Organized League Player</td></tr>
        <tr><td>
        <?php echo  my_radio(array('name'  => 'category',
                             'id'    => 'category',
                             'checked' => FALSE,
                             'value' => 3,
                       ));
        ?></td><td>Experienced Organized League Player</td></tr>
        <tr><td>
        <?php echo  my_radio(array('name'  => 'category',
                             'id'    => 'category',
                             'checked' => FALSE,
                             'value' => 4,
                       ));
        ?></td><td>Experienced Club Player (8 tournament minimum)</td></tr>
        <tr><td>
        <?php echo  my_radio(array('name'  => 'category',
                             'id'    => 'category',
                             'checked' => FALSE,
                             'value' => 5,
                       ));
        ?></td><td>Franchise Club Team Player (the 90th percentile player)</td></tr>
        </table>
    </td></tr>
    <tr><td colspan=3><br>
    Club Experience: If you've played club ultimate, list the last club team you played on.
    </td></tr>
    <tr><td>
    Team name/location:
    </td><td> </td>
    </td><td>
        <?php echo  my_input(array('name'      => 'club_experience',
                             'id'        => 'club_experience',
                             'maxlength' => '1024',
                             'size'      => $def_size,
                       ));
        ?>
    </td></tr>
    <tr><td>
    League Selection:
    </td><td>*</td>
    </td><td>
        <table><tr><td>
        <?php echo  my_checkbox(array('name'  => 'league1',
                                'id'    => 'league1',
                                'value' => '1',
                                'checked' => FALSE,
                       ));
        ?></td><td>
Monday Night Co-ed Draft League:
        </td></tr><tr><td>
        <?php echo  my_checkbox(array('name'  => 'league2',
                                'id'    => 'league2',
                                'value' => '2',
                                'checked' => FALSE,
                       ));
        ?></td><td>
Wednesday Night Co-ed Clique &amp; Pick League:
        </td></tr></table>
    </td></tr>
    <tr><td colspan=3><br>
Optional Chum Requests - name of one Significant Other (or one friend if you are
a novice) that you would like as a teammate.
    </td></tr>
    <tr><td>Monday Night Chum Request:    
    </td><td> </td>
    </td><td>
        <?php echo  my_input(array('name'        => 'baggage',
                             'id'        => 'baggage',
                             'maxlength' => '1024',
                             'size'      => $def_size,
                       ));
        ?>
    </td></tr>
    <tr><td>Wednesday Night Chum Request:    
    </td><td> </td>
    </td><td>
        <?php echo  my_input(array('name'        => 'baggage2',
                             'id'        => 'baggage2',
                             'maxlength' => '1024',
                             'size'      => $def_size,
                       ));
        ?>
    </td></tr>
    <tr><td>
Exceptions: Dates you'll be late or unable to play:
    </td><td> </td>
    </td><td>
        <?php echo my_textarea(array('name'      => 'exceptions',
                                     'id'        => 'exceptions',
                                     'rows'      => 5,
                                     'cols'      => 40,
                       ));
        ?>
    </td></tr>
    <tr><td colspan=3><br><br>
- SUMMER LEAGUE WAIVER -<br>
PLEASE READ THIS IMPORTANT INFORMATION AND SIGN BELOW<br>
I acknowledge that an Ultimate event is an extreme test of a person's physical
and mental limits and carries with it the potential for death, serious injury
and property loss.  I HEREBY ASSUME THE RISKS OF PARTICIPATING IN ULTIMATE
GAMES, TOURNAMENTS, PRACTICES AND OTHER EVENTS. I certify that I am physically
fit, have trained sufficiently for participation in Ultimate events and have
not been advised against participation in Ultimate events by a qualified
medical person.  I acknowledge that my statements on this Acknowledgment Waiver
and Release Form are being accepted by the Central Maryland Ultimate
Association (CMUA) in consideration for allowing me to become a member of the
CMUA and are being relied upon by the CMUA and the various event sponsors,
organizers and administrators in permitting me to participate in any CMUA
sanctioned tournament, practice or other event.<br><br>

In consideration for allowing
me to become a member of the CMUA and allowing me to participate in CMUA
sanctioned events, I hereby take the following action for myself, my executors,
administrators, heirs, next of kin, successors and assigns: (a) I AGREE to
abide by the rules of Ultimate and the Bylaws adopted by the CMUA and I
acknowledge that my membership may be revoked or suspended for violations
thereof; (b) I WAIVE, RELEASE AND DISCHARGE from any and all claims or
liabilities for death, personal injury, property damage, theft or damages of
any kind, which arise out of or relate to my participation in CMUA sanctioned
events, THE FOLLOWING PERSONS OR ENTITIES: the CMUA, event sponsors, tournament
directors, event producers, volunteers, all states, cities, counties or
localities in which events or segments of events are held, and the officers,
directors, employees, representatives and agents of any of the above; (c) I
AGREE NOT TO SUE any of the persons or entities mentioned above for any of the
claims or liabilities that I have waived, released or discharged herein; and
(d) I INDEMNIFY AND HOLD HARMLESS the persons or entities mentioned above from
any claims made or liabilities assessed against them as a result of my actions
during a CMUA sanctioned event.<br><br>
    </tr><tr><td colspan=3>
        <table><tr><td width=40>
        <?php echo  form_checkbox(array('name'  => 'waiver',
                                'id'    => 'waiver',
                                'value' => '3',
                                'checked' => FALSE,
                       ));
        ?>
        </td><td>
I HEREBY AFFIRM THAT I AM EIGHTEEN (18) YEARS
OF AGE OR OLDER, I HAVE READ THIS DOCUMENT AND I UNDERSTAND ITS CONTENTS.
        </td></tr></table>
    </td></tr><tr><td colspan=3>
(The
checkmark in this box serves as your electronic signature and is required
before the registration can be completed.)<br>
<br><a name="adob"><sup>&#x2020;</sup></a>Players under the age of 18 must download 
and use the <a
href="/static/2008Summer_Registration_Form.pdf">paper registration</a> 
(also available as a <a href="/static/2008Summer_Registration_Form.doc">word document</a>) form in 
order to provide proper parent/guardian authorization.<br><br>
    </td></tr>    
    <tr><td>
    <?php echo  form_submit(array('name'  => 'submit',
                          'id'    => 'submit',
                          'value' => 'Submit registration',
                    ));
    ?>
    </td></tr></table>
<?php echo  form_close(); ?>
    </div>
</div>
<?php include_once('foot.php') ?>
