# Introduction
Simple receiver of ODF messages, compliant with odf.olympictech.org 

# Features

* Decoupled message receiving and processing. An HTTP response is sent
  immediately upon receving a message, to avoid stalling the sender. Message
  processing is done in a separate thread

* Messages are stored in three folders
  * `tmp` Stores messages when they are received. Processed messages can be
    moved to this folder for reprocessing. File name is the received timestamp

  * `messages` Processed messages, grouped in folders by day. File name
    includes ODF header attribues and the received timestamp

  * `error` Files in `tmp` folder having an unrecognized file name

* `tmp` folder is monitored for new files, any file copied in that folder
  will be processed. Useful to reprocess messages

* Full help provided by `--help` option

* Implemented HTTP paths and methods

  * `GET /` Provides a help message to validate that application is up and running

  * `POST /` Consumes the request and ignores it. Useful as dummy client just
    to validate end-to-end sender

  * `POST /odf` ODF Messages are stored and processed

# FAQ

## Why use file system to store messages between threads instead of using a queue?

Two reasons:

  1. Allows to reprocessing messages by just putting them in the tmp folder

  2. If the processing is slow, a crash of the app would not result in lost messages

This app is just an example, a template which can be extended with now complex
(slower) processing, such as saving to DB
