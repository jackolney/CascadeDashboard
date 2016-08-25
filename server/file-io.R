output$downloadMasterDataSet <- downloadHandler(
    filename = 'MasterData.RData',
    content = function(file) {
        message("trying to save data.")
        if (exists("MasterData")) {
            save(MasterData, file = file)
        }
    }
)
