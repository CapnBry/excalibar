cd ..
del CamelotChatRT-src.zip
del CamelotChatRT.zip
wzzip -ex -aP CamelotChatRT-src.zip @CamelotChatRT\ccrt_source_files.txt
wzzip -ex -aP CamelotChatRT.zip @CamelotChatRT\ccrt_dist_files.txt
cd CamelotChatRT
