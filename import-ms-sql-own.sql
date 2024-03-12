ALTER DATABASE [chatDb] SET COMPATIBILITY_LEVEL = 100
GO
IF (1 = FULLTEXTSERVICEPROPERTY('IsFullTextInstalled'))
begin
EXEC [chatDB].[dbo].[sp_fulltext_database] @action = 'enable'
end
GO
ALTER DATABASE [chatDB] SET ANSI_NULL_DEFAULT OFF
GO
ALTER DATABASE [chatDB] SET ANSI_NULLS OFF
GO
ALTER DATABASE [chatDB] SET ANSI_PADDING OFF
GO
ALTER DATABASE [chatDB] SET ANSI_WARNINGS OFF
GO
ALTER DATABASE [chatDB] SET ARITHABORT OFF
GO
ALTER DATABASE [chatDB] SET AUTO_CLOSE OFF
GO
ALTER DATABASE [chatDB] SET AUTO_CREATE_STATISTICS ON
GO
ALTER DATABASE [chatDB] SET AUTO_SHRINK OFF
GO
ALTER DATABASE [chatDB] SET AUTO_UPDATE_STATISTICS ON
GO
ALTER DATABASE [chatDB] SET CURSOR_CLOSE_ON_COMMIT OFF
GO
ALTER DATABASE [chatDB] SET CURSOR_DEFAULT  GLOBAL
GO
ALTER DATABASE [chatDB] SET CONCAT_NULL_YIELDS_NULL OFF
GO
ALTER DATABASE [chatDB] SET NUMERIC_ROUNDABORT OFF
GO
ALTER DATABASE [chatDB] SET QUOTED_IDENTIFIER OFF
GO
ALTER DATABASE [chatDB] SET RECURSIVE_TRIGGERS OFF
GO
ALTER DATABASE [chatDB] SET  DISABLE_BROKER
GO
ALTER DATABASE [chatDB] SET AUTO_UPDATE_STATISTICS_ASYNC OFF
GO
ALTER DATABASE [chatDB] SET DATE_CORRELATION_OPTIMIZATION OFF
GO
ALTER DATABASE [chatDB] SET TRUSTWORTHY OFF
GO
ALTER DATABASE [chatDB] SET ALLOW_SNAPSHOT_ISOLATION OFF
GO
ALTER DATABASE [chatDB] SET PARAMETERIZATION SIMPLE
GO
ALTER DATABASE [chatDB] SET READ_COMMITTED_SNAPSHOT OFF
GO
ALTER DATABASE [chatDB] SET HONOR_BROKER_PRIORITY OFF
GO
ALTER DATABASE [chatDB] SET  READ_WRITE
GO
ALTER DATABASE [chatDB] SET RECOVERY SIMPLE
GO
ALTER DATABASE [chatDB] SET  MULTI_USER
GO
ALTER DATABASE [chatDB] SET PAGE_VERIFY CHECKSUM
GO
ALTER DATABASE [chatDB] SET DB_CHAINING OFF
GO
USE [chatDB]
GO
/****** Object:  Table [dbo].[participants]    Script Date: 01/21/2020 18:47:40 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[participants](
	[id] [int] IDENTITY(1,1) NOT NULL,
	[username] [nvarchar](200) NOT NULL,
	[passwd] [varbinary](20) NULL,
	[salt] [varbinary](25) NULL,
 CONSTRAINT [PK_participants] PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY],
 CONSTRAINT [AK_username] UNIQUE NONCLUSTERED 
(
	[username] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
/****** Object:  UserDefinedFunction [dbo].[test]    Script Date: 01/21/2020 18:47:40 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE FUNCTION [dbo].[test] () 
RETURNS BIT
AS
BEGIN
  RETURN 0;
END;
GO
/****** Object:  UserDefinedFunction [dbo].[validate_user]    Script Date: 01/21/2020 18:47:40 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE FUNCTION [dbo].[validate_user]
(
  @_username NVARCHAR(200),
  @_passwd NVARCHAR(255)
	
)
RETURNS BIT
AS
BEGIN
  DECLARE @passwd1 VARBINARY(20);
  DECLARE @salt1 VARBINARY(25);
  
  SET @passwd1 = null;
  SELECT TOP 1 @passwd1 = passwd, @salt1 = salt 
    FROM participants
    WHERE username = @_username;
    
  IF @passwd1 IS null
    RETURN 0;  
  
  DECLARE @hash VARBINARY(20);
  SET @hash = HASHBYTES('SHA2_256', @_passwd + CAST(@salt1 AS NVARCHAR(25)));
  IF (@hash = @passwd1) 
    RETURN 1
  RETURN 0;
END
GO
/****** Object:  StoredProcedure [dbo].[reset_password]    Script Date: 01/21/2020 18:47:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[reset_password]
(
	@_username NVARCHAR(200),
	@_password NVARCHAR(255)
)
AS
BEGIN
  DECLARE @salt1 VARBINARY(25);
  SET @salt1 = CAST(RAND() AS BINARY(4)) + 
              CAST(RAND() AS BINARY(7)) +
              CAST(RAND() AS BINARY(7)) +
              CAST(RAND() AS BINARY(7));
  DECLARE @hash VARBINARY(20);
  SET @hash = HASHBYTES('SHA2_256', @_password + CAST(@salt1 AS NVARCHAR(25)));
  UPDATE participants
  SET passwd = @hash, salt = @salt1
  WHERE username = @_username;   
END
GO
/****** Object:  Table [dbo].[history]    Script Date: 01/21/2020 18:47:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[history](
	[id] [int] IDENTITY(1,1) NOT NULL,
	[sender] [int] NOT NULL,
	[message] [nvarchar](max) NULL,
	[time_send] [datetime] NULL,
 CONSTRAINT [PK_history] PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  StoredProcedure [dbo].[add_user]    Script Date: 01/21/2020 18:47:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[add_user]
	@_username NVARCHAR(200),
	@_password NVARCHAR(255)
AS
BEGIN
-- Заносим в таблицу криптографическую соль и хэш
  DECLARE @salt VARBINARY(25);
  SET @salt = CAST(RAND() AS BINARY(4)) + 
              CAST(RAND() AS BINARY(7)) +
              CAST(RAND() AS BINARY(7)) +
              CAST(RAND() AS BINARY(7));
  DECLARE @hash VARBINARY(20);
  SET @hash = HASHBYTES('SHA2_256', @_password + CAST(@salt AS NVARCHAR(25)));
  INSERT INTO participants (username, passwd, salt) VALUES
  (@_username, @hash, @salt);
END
GO
/****** Object:  ForeignKey [FK_SENDER]    Script Date: 01/21/2020 18:47:41 ******/
ALTER TABLE [dbo].[history]  WITH CHECK ADD  CONSTRAINT [FK_SENDER] FOREIGN KEY([sender])
REFERENCES [dbo].[participants] ([id])
ON UPDATE CASCADE
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[history] CHECK CONSTRAINT [FK_SENDER]
GO
