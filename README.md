Программа предназначена для мгновенного обмена сообщениями с централизованным хранением истории в БД . Используется шифрование соедниений посредством OpenSSL, а также проверка подлинности сервера.
Использование:
1. ChatServer.exe -start \[<ip_address>\] \<port\> \[\<setting_file_name\> \<filekey\> \<filecert\>\] - запуск сервера по адресу \<IP address\> с прослушиванием TCP-порта \<port\>. Файл настроек \<setting_file_name\> указывает на файл с настройками подключения к СУБД. Если этот параметр не указан, будет искаться файл SqlSettings.conf у испольняемого файла. В случае отсутствия файла \<setting_file_name\>, то создаться файл с содеражнием по-умолчанию.
Путь к файлам с закрытым ключом и сертификатом устанавливаются параметрами \<filekey\> и \<filecert\> соответсвенно. Если пропустить эти параметры, будут использоваться файлы cert.pem и key.pem при их наличии. Если же таких файлов нет, сервер будет работать без защиты сеансов.
2. ChatServer.exe  { -adduser | -removeuser | -resetuser } \[\<setting_file_name\>\] \<username\> -  добавление (-add), удаление (-remove) пользователя с именем  \<username\>, а также сброс (-reset) пароля у \<username\>. Обработка параметра \<setting_file_name\> аналогично п.1

На данный момент реализованf поддержка следующих СУБД
1. MS-SQL с собстенным управлением пользователями и аутентификацией (таблица participants; см. файл экспорта export-ms-sql-own.sql)
