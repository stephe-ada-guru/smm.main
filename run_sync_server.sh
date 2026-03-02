export SERVER_IP=127.0.0.1
export SERVER_PORT=16#9002#
make -f Alire.make empty_database_test_1
make -f Alire.make empty_database_test_2
build/bin/smm-db_sync_server.exe smm_test_1.config 2
