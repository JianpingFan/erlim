/*
Navicat MySQL Data Transfer

Source Server         : 127.0.01
Source Server Version : 80012
Source Host           : localhost:3306
Source Database       : erlim

Target Server Type    : MYSQL
Target Server Version : 80012
File Encoding         : 65001

Date: 2020-03-13 23:57:50
*/

SET FOREIGN_KEY_CHECKS=0;

-- ----------------------------
-- Table structure for users
-- ----------------------------
DROP TABLE IF EXISTS `users`;
CREATE TABLE `users` (
  `user_id` int(11) NOT NULL,
  `mobile` bigint(11) NOT NULL,
  `pwd` varchar(255) NOT NULL,
  `nickname` varchar(255) NOT NULL,
  `create_time` int(11) NOT NULL,
  PRIMARY KEY (`user_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='用户表';

-- ----------------------------
-- Records of users
-- ----------------------------
INSERT INTO `users` VALUES ('3', '17380630290', '123456', 'fanjianping', '1582444902');
INSERT INTO `users` VALUES ('7', '17380630291', '123456', 'fanjianping', '1582464815');
