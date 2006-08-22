

# phpMyAdmin MySQL-Dump
# version 2.2.6
# http://phpwizard.net/phpMyAdmin/
# http://www.phpmyadmin.net/ (download page)
#
# Host: localhost
# Generation Time: Aug 09, 2002 at 07:36 PM
# Server version: 3.23.47
# PHP Version: 4.2.2
# Database : `llimllib`
# --------------------------------------------------------

#
# Table structure for table `blog`
#

CREATE TABLE blog (
  blog_id int(5) NOT NULL auto_increment,
  blog_date date default NULL,
  title varchar(50) default '''''',
  blogtext text NOT NULL,
  PRIMARY KEY  (blog_id)
) TYPE=MyISAM;

#
# Dumping data for table `blog`
#

INSERT INTO blog VALUES (1, '2002-06-14', '\'\'', '<p>The web page is actually getting done, and not by itself. I don\'t have work for a few days, so I\'m going to try to port the old site to this one in the upcoming few days.</p>\r\n<p>In other news, the World Cup has been affecting the speed at which this webpage comes along. Instead of working, I\'ve been reading analyses of the Cup...oh well. The US is <a class=link href="http://worldcup.espnsoccernet.com/report?match=48855&lang=en">in the second round</a> thanks to some clutch scoring by <a href="http://worldcup.espnsoccernet.com/report?match=48854&lang=en" class="link">South Korea</a> and some stupid play by Portugal, but they\'ll take what they can get.</p>\r\n<p>Another ultimate frisbee game for me tomorrow morning, then driving home for my bro\'s graduation. Should be nice to see my folks again, it\'s been a while.</p>');
INSERT INTO blog VALUES (2, '2002-06-19', '\'\'', '<p>I ran across an interesting <a href="http://www.perl.com/pub/a/1999/03/pm.html" class="link"> article</a> today. Written by Larry Wall (creator of perl), it discusses      postmodernism and its relationship to science, more particularly its relationship       to computer science and programming language design. It\'s interesting how he combines philosophy and literary theory with technology - makes me think of a <a href="http://www.biota.org/people/douglasadams/index.html" class="link">speech</a> I bookmarked by Douglas Adams. This is where I want to go with research into computer science - how it relates to the world at large, as opposed to its application to specific problems.</p>\r\n<p>More personally, I enjoyed my brother\'s graduation, it was nice to be home for a weekend. I have a <a href="http://www.pada.org" class="link">frisbee</a> game tomorrow after a heartbreaking loss on Saturday, hopefully we\'ll rebound. </p>\r\n<p>Speaking of sports, I spent Sunday morning from 2:30 to 4:30 watching the \r\n<a href="http://worldcup.espnsoccernet.com/story?id=217483&lang=en" class="link">US trash a poor Mexico side</a> in the World Cup. I\'m amazed I didn\'t wake up my parents (as i was home for the weekend) jumping around at 4:00 when <a href="http://worldcup.espnsoccernet.com/player?id=20108&lang=en" class="link">Landon Donovan</a> scored the 2nd goal.</p>');
INSERT INTO blog VALUES (3, '2002-07-10', '\'\'', '<p>I now have a new hero. Former stanford student <a href="http://www.stanford.edu/~dgi/" class="link">Daniel Gomez Ibanez</a> was featured on <a href="http://www.slashdot.com" class="link">Slashdot</a> this evening for his design - a simulated turntable driven off a CD player. Storing data from the CD in RAM, it lets you scratch a surface that looks like a white record. Pictures and a link to a movie of it in action can be found <a href="http://www.stanford.edu/~dgi/university/" class="link">here</a>.\r\nOn another note, I\'m back to working on the website, trying to keep it up to date, and trying to get everything on here. It\'s been slowed down recently by the projects I\'ve been working on.</p>\r\n<p>Last week, I wrote the first part of a tutorial on creating a PHP login script. When (if) that gets published, hopefully at phpcomplete.com. My FarKommander project is done, because Drew added the features I was trying (and almost completely done with) to the page. Finally, My current project is trying to create a PHP <a href="http://www.webdav.org" class="link">webDAV</a> client, which will eventually allow me to access my Hotmail account without using either of their godawful clients.</p>\r\n<p>To do this, I\'m building off work already done <a href="http://jhttpmail.sourceforge.net/httpmail.html" class="link">here</a> and <a href="http://sourceforge.net/projects/httpmail" class="link">here</a>. Basically, Microsoft doesn\'t allow POP access to their server, but they do allow webDAV, a new protocol, to access the server. This is how Outlook Express connects to their server, and it has been reverse engineered by the aforementioned projects.</p>\r\n<p>Finally, I\'ve been out enjoying the summer, playing frisbee, and going to the beach. I had a good 4th of July, winning a frisbee tournament, and went the following weekend to see my family in New Jersey.</p>');
# --------------------------------------------------------

#
# Table structure for table `user`
#

CREATE TABLE user (
  user_name varchar(15) NOT NULL default '',
  password varchar(32) NOT NULL default '',
  permission char(1) NOT NULL default '0',
  add_date datetime NOT NULL default '0000-00-00 00:00:00',
  PRIMARY KEY  (user_name)
) TYPE=MyISAM;

#
# Dumping data for table `user`
#

INSERT INTO user VALUES ('wimill', '19c25392696a16e7dc1946ad33576d85', 'A', '2002-08-09 17:11:23');