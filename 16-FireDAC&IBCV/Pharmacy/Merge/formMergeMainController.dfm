object MergeMainController: TMergeMainController
  OldCreateOrder = False
  Height = 150
  Width = 215
  object ActionList: TActionList
    Left = 88
    Top = 56
    object actMerge: TAction
      Caption = 'Merge'
      OnExecute = actMergeExecute
    end
    object actGetIBFileName: TAction
      Caption = '...'
      OnExecute = actGetIBFileNameExecute
    end
  end
end
