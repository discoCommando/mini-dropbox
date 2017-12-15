CREATE TABLE folders(
    folderId BIGSERIAL PRIMARY KEY,
    folderParentId INT NOT NULL,
    folderName TEXT NOT NULL, 
    folderUid TEXT NOT NULL,
    folderInsertDate TIMESTAMPTZ NOT NULL
);

CREATE TABLE files(
    fileId BIGSERIAL PRIMARY KEY,
    fileFolderId INT NOT NULL, 
    fileName TEXT NOT NULL, 
    fileUid TEXT NOT NULL,
    fileSize INT NOT NULL,
    fileInsertDate TIMESTAMPTZ NOT NULL   
);
