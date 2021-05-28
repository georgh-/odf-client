# Revision history for odf-client

## 0.3.0.0 -- 2021-05-28

* Migrated to new date format for received temporary files
  This format ensures that:
    - Files can be copied directly to Windows (removed ':')
    - Time zone is now numeric (more resilient parsing)
    - Sub-second values now takes a fixed length, instead of 
      removing trailing zeroes

## 0.2.0.0 -- 2021-05-28

* Added ODF header naming on file name and extension .xml or .xml.gz
* Automatic creation of folders if don't exist
* Messages are classified by date

## 0.1.0.0 -- 2021-05-04

* First version, ready for Beijing 2022 MST1. It just stores the messages
