-- phpMyAdmin SQL Dump
-- version 2.8.1
-- http://www.phpmyadmin.net
-- 
-- Host: localhost
-- Generation Time: Dec 12, 2007 at 05:42 AM
-- Server version: 4.1.13
-- PHP Version: 5.1.4
-- 
-- Database: `mdultimate`
-- 

-- --------------------------------------------------------

-- 
-- Table structure for table `News`
-- 

DROP TABLE IF EXISTS `News`;
CREATE TABLE `News` (
  `id` int(11) NOT NULL auto_increment,
  `title` text NOT NULL,
  `body` text NOT NULL,
  `author` bigint(20) NOT NULL default '0',
  `newsdt` timestamp NOT NULL default CURRENT_TIMESTAMP,
  PRIMARY KEY  (`id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 AUTO_INCREMENT=7 ;

-- 
-- Dumping data for table `News`
-- 

INSERT INTO `News` (`id`, `title`, `body`, `author`, `newsdt`) VALUES (1, 'This is the first news item', 'This is news!', 0, '2006-11-20 05:00:00'),
(3, 'This is another news item', 'I''m so damn meta you can''t even handle it.', 0, '2006-11-22 13:32:10'),
(4, 'New News!', 'Things have happened!', 0, '2006-11-22 13:33:03'),
(5, 'this one''s gonna work', 'I think?', 0, '2006-11-24 04:22:16'),
(6, 'This is the demo site', 'The demo is now working on my personal web site.', 0, '2006-11-29 05:34:57');

-- --------------------------------------------------------

-- 
-- Table structure for table `ci_sessions`
-- 

DROP TABLE IF EXISTS `ci_sessions`;
CREATE TABLE `ci_sessions` (
  `session_id` varchar(40) NOT NULL default '0',
  `ip_address` varchar(16) NOT NULL default '0',
  `user_agent` varchar(50) NOT NULL default '',
  `last_activity` int(10) unsigned NOT NULL default '0',
  `session_data` text NOT NULL,
  PRIMARY KEY  (`session_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

-- 
-- Dumping data for table `ci_sessions`
-- 

INSERT INTO `ci_sessions` (`session_id`, `ip_address`, `user_agent`, `last_activity`, `session_data`) VALUES ('63fe2d50e80e1cd3f35f507a5ff986a1', '71.200.92.239', 'Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv', 1164901796, ''),
('c940f26640626532ab4e53fc67d11220', '71.200.92.239', 'Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv', 1165445027, 'a:2:{s:9:"LOGGED_IN";s:5:"guest";s:12:"GROUP_Member";s:4:"true";}'),
('07b2aa14f5891c5f691576630d626c0c', '68.34.6.172', 'Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.8.1)', 1164857739, ''),
('69563d2f48da964128cbdd2b1bda887f', '74.6.86.22', 'Mozilla/5.0 (compatible; Yahoo! Slurp; http://help', 1166104655, ''),
('c6c9337a263693b46f779126282414c0', '74.6.85.147', 'Mozilla/5.0 (compatible; Yahoo! Slurp; http://help', 1166104745, ''),
('2262df8bdeb7949ded4efb2d18853651', '74.6.87.116', 'Mozilla/5.0 (compatible; Yahoo! Slurp; http://help', 1166104826, ''),
('d94c86e1b4c96a1e1159dc53146952c2', '74.6.86.22', 'Mozilla/5.0 (compatible; Yahoo! Slurp; http://help', 1170575430, ''),
('4c31db536296863ed31dbadf37089397', '74.6.85.147', 'Mozilla/5.0 (compatible; Yahoo! Slurp; http://help', 1170712222, ''),
('1d7ff678fcf3c0b7f5739a62bd2d08ad', '74.6.85.160', 'Mozilla/5.0 (compatible; Yahoo! Slurp; http://help', 1170712993, ''),
('6a898ac354325d1349a575126cc972c7', '66.249.66.172', 'Mozilla/5.0 (compatible; Googlebot/2.1; +http://ww', 1170783366, ''),
('dc5d2172068ac02662c9acb90b835d6f', '75.134.223.54', 'Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv', 1174324299, ''),
('4a0b08bf7930256d03a9450e919ff25a', '74.6.86.11', 'Mozilla/5.0 (compatible; Yahoo! Slurp; http://help', 1174834522, ''),
('cfb83508ee18f02f623a8507088e4912', '66.249.66.146', 'Mozilla/5.0 (compatible; Googlebot/2.1; +http://ww', 1175689355, ''),
('3d728c2b973d7013d33891d18fa38667', '66.249.66.51', 'Mozilla/5.0 (compatible; Googlebot/2.1; +http://ww', 1175832414, ''),
('4de4d425a1c9c6fe96796258415a7e61', '66.249.66.110', 'Mozilla/5.0 (compatible; Googlebot/2.1; +http://ww', 1178988080, ''),
('b88ede4e32c73b25efed5f5abcde6941', '66.249.66.129', 'Mozilla/5.0 (compatible; Googlebot/2.1; +http://ww', 1179660152, ''),
('c13cd79f1fe05252d6b9d9d9b0a6513f', '66.249.66.81', 'Mozilla/5.0 (compatible; Googlebot/2.1; +http://ww', 1181670279, ''),
('1c4c67a048c043f9d2530469f4ba36ca', '66.249.66.81', 'Mozilla/5.0 (compatible; Googlebot/2.1; +http://ww', 1182244167, ''),
('2e510e8f632c3d596468fe3225cfd69a', '66.249.73.213', 'Mozilla/5.0 (compatible; Googlebot/2.1; +http://ww', 1185353982, ''),
('444d404dc6068c7bb9d678ba37c054ac', '66.249.73.213', 'Mozilla/5.0 (compatible; Googlebot/2.1; +http://ww', 1185564718, ''),
('917b397faa6735a01ceb943d6a79b696', '66.249.73.213', 'Mozilla/5.0 (compatible; Googlebot/2.1; +http://ww', 1188293004, ''),
('50a56c155a27ec7d23f58721dfbeb175', '66.249.73.213', 'Mozilla/5.0 (compatible; Googlebot/2.1; +http://ww', 1189501811, ''),
('3d59fe3109b0ccad0a207f0e37034bf6', '66.249.73.213', 'Mozilla/5.0 (compatible; Googlebot/2.1; +http://ww', 1192007684, ''),
('07d00c1e5ccfcf9edd5e2ca3af643b31', '66.249.73.213', 'Mozilla/5.0 (compatible; Googlebot/2.1; +http://ww', 1193714308, ''),
('ccd090b3607ebbbaf959d2eb620a4d49', '66.249.73.213', 'Mozilla/5.0 (compatible; Googlebot/2.1; +http://ww', 1195537404, ''),
('b2504f1373527dbf8088d78d67d063b1', '66.249.73.213', 'Mozilla/5.0 (compatible; Googlebot/2.1; +http://ww', 1196839799, '');

-- --------------------------------------------------------

-- 
-- Table structure for table `security_role`
-- 

DROP TABLE IF EXISTS `security_role`;
CREATE TABLE `security_role` (
  `id` bigint(20) NOT NULL auto_increment,
  `name` varchar(45) NOT NULL default '',
  `created` timestamp NOT NULL default CURRENT_TIMESTAMP on update CURRENT_TIMESTAMP,
  `modified` timestamp NOT NULL default '0000-00-00 00:00:00',
  PRIMARY KEY  (`id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 AUTO_INCREMENT=3 ;

-- 
-- Dumping data for table `security_role`
-- 

INSERT INTO `security_role` (`id`, `name`, `created`, `modified`) VALUES (1, 'Admin', '2006-11-17 16:08:28', '0000-00-00 00:00:00'),
(2, 'Member', '2006-11-17 16:08:28', '0000-00-00 00:00:00');

-- --------------------------------------------------------

-- 
-- Table structure for table `security_role_user`
-- 

DROP TABLE IF EXISTS `security_role_user`;
CREATE TABLE `security_role_user` (
  `id` bigint(20) NOT NULL auto_increment,
  `security_role_id` bigint(20) NOT NULL default '0',
  `user_id` bigint(20) NOT NULL default '0',
  PRIMARY KEY  (`id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 AUTO_INCREMENT=4 ;

-- 
-- Dumping data for table `security_role_user`
-- 

INSERT INTO `security_role_user` (`id`, `security_role_id`, `user_id`) VALUES (1, 2, 1),
(2, 1, 1),
(3, 2, 2);

-- --------------------------------------------------------

-- 
-- Table structure for table `user`
-- 

DROP TABLE IF EXISTS `user`;
CREATE TABLE `user` (
  `id` bigint(20) NOT NULL auto_increment,
  `user_name` varchar(45) NOT NULL default '',
  `password` varchar(40) NOT NULL default '',
  `email` varchar(120) NOT NULL default '',
  `activated` tinyint(1) NOT NULL default '0',
  `activation_code` varchar(50) default NULL,
  `forgotten_password_code` varchar(50) default NULL,
  `last_visit` datetime default NULL,
  `created` timestamp NOT NULL default CURRENT_TIMESTAMP on update CURRENT_TIMESTAMP,
  `modified` timestamp NOT NULL default '0000-00-00 00:00:00',
  `name` varchar(120) NOT NULL default '',
  `street_addr1` varchar(255) NOT NULL default '',
  `street_addr2` varchar(255) NOT NULL default '',
  `phone` varchar(25) NOT NULL default '',
  `zip` varchar(10) NOT NULL default '',
  `city` varchar(255) NOT NULL default '',
  `state` char(2) NOT NULL default '',
  PRIMARY KEY  (`id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 AUTO_INCREMENT=3 ;

-- 
-- Dumping data for table `user`
-- 

INSERT INTO `user` (`id`, `user_name`, `password`, `email`, `activated`, `activation_code`, `forgotten_password_code`, `last_visit`, `created`, `modified`, `name`, `street_addr1`, `street_addr2`, `phone`, `zip`, `city`, `state`) VALUES (1, 'llimllib', '2836ccbed1d3c5aa8918a926292d120b5d70bed6', 'bill.mill@gmail.com', 1, '', '', '0000-00-00 00:00:00', '2006-11-20 16:46:44', '0000-00-00 00:00:00', 'Bill Mill', '23 Foster Ave', '', '8608823587', '21224', 'Baltimore', 'MD'),
(2, 'guest', '35675e68f4b5af7b995d9205ad0fc43842f16450', 'user@example.com', 1, '', '', '0000-00-00 00:00:00', '2006-11-20 23:08:17', '0000-00-00 00:00:00', 'Just a User', '23 Foster Ave', '', '8602248483', '21224', 'Baltimore', 'MD');

DROP TABLE IF EXISTS `registrant`;
CREATE TABLE `registrant` (
    `id` bigint unsigned NOT NULL PRIMARY KEY auto_increment,
    `player_id` bigint unsigned NOT NULL,
    `league_id` bigint unsigned NOT NULL,
    `team_id` bigint unsigned default NULL,
    `category` tinyint unsigned NOT NULL,
    `rating` float default -1,
    `last_club` varchar(255) default '',
    `unable_to_play` text default ''
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

DROP TABLE IF EXISTS `player`;
CREATE TABLE `player` (
    `id` bigint unsigned NOT NULL PRIMARY KEY auto_increment,
    `name` text NOT NULL,
    `address` text NOT NULL,
    `city` varchar(255) NOT NULL,
    `state` char(2) NOT NULL,
    `zip` char(5) NOT NULL,
    `home_phone` varchar(20),
    `work_phone` varchar(20),
    `cell_phone` varchar(20),
    `birthdate` date NOT NULL,
    `gender` tinyint,
    `email_address` varchar(255)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

DROP TABLE IF EXISTS `team`;
CREATE TABLE `team` (
    `id` bigint unsigned NOT NULL PRIMARY KEY auto_increment,
    `league_id` bigint unsigned NOT NULL,
    `name` varchar(255) NOT NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

DROP TABLE IF EXISTS `league`;
CREATE TABLE `league` (
    `id` bigint unsigned NOT NULL PRIMARY KEY auto_increment,
    `name` varchar(255) NOT NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

-- temporary table for quick registrations
DROP TABLE IF EXISTS `register`;
CREATE TABLE `register` (
    name varchar(1024),
    address varchar(1024),
    city varchar(1024),
    state varchar(1024),
    zip varchar(1024),
    phone1 varchar(1024),
    phone2 varchar(1024),
    dob varchar(1024),
    gender varchar(1024),
    mailing_address varchar(1024),
    category varchar(1024),
    rating varchar(1024),
    club_experience varchar(1024),
    baggage varchar(1024),
    league varchar(1024)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
