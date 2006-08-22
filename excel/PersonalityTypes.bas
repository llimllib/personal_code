Attribute VB_Name = "PersonalityTypes"
Dim description As String

Function ISTJParagraph(Qfirst, QandA)
'" & " stinking newline character or Chr(13)
If Qfirst Then
    description = "Inspectors are law-abiding, no-nonsense, ""superdependable"" workers." _
   & "  They carefully and thoroughly examine products and accounts of an institution without" _
   & " flourish or fanfare.   Of all the types, they are the most realistic, down-to-earth" _
   & " and practical.  They are pessimistic and risk-averse by nature and are very" _
   & " traditional and conservative in their viewpoints.  They are dedicated and highly" _
   & " organized workers."
   
Select Case QandA
    Case "QG"
lastSentence = "The ""QG"" Inspector focuses on the task close at hand and is adept at" _
& " inspection planning and analysis."
    Case "QQG"
lastSentence = "The ""QQG"" Inspector focuses on the task close at hand and is particularly adept at" _
& " inspection planning and analysis."
    Case "QQQG"
lastSentence = "The ""QQQG"" Inspector focuses on the task close at hand and is extremely adept at" _
& " inspection planning and analysis."
    Case "QGG"
lastSentence = "The ""QGG"" Inspector focuses very closely on the task at hand and is adept at" _
& " inspection planning and analysis."
    Case "QQGG"
lastSentence = "The ""QQGG"" Inspector focuses very closely on the task at hand and is particularly adept at" _
& " inspection planning and analysis."
    Case "QQQGG"
lastSentence = "The ""QQQGG"" Inspector focuses very closely on the task at hand and is extremely adept at" _
& " inspection planning and analysis."
    Case "QGGG"
lastSentence = "The ""QGGG"" Inspector focuses extremely closely on the task at hand and is adept at" _
& " inspection planning and analysis."
    Case "QQGGG"
lastSentence = "The ""QQGGG"" Inspector focuses extremely closely on the task at hand and is particularly adept at" _
& " inspection planning and analysis."
    Case "QQQGGG"
lastSentence = "The ""QQQGGG"" Inspector focuses extremely closely on the task at hand and is exceptionally adept at" _
& " inspection planning and analysis."
    Case "QB"
lastSentence = "The ""QB"" Inspector sees the big picture and is adept at" _
& " inspection planning and analysis."
    Case "QQB"
lastSentence = "The ""QQB"" Inspector sees the big picture and is particularly adept at" _
& " inspection planning and analysis."
    Case "QQQB"
lastSentence = "The ""QQQB"" Inspector sees the big picture and is extremely adept at" _
& " inspection planning and analysis."
    Case "QBB"
lastSentence = "The ""QBB"" Inspector sees the big picture clearly and is adept at" _
& " inspection planning and analysis."
    Case "QQBB"
lastSentence = "The ""QQBB"" Inspector sees the big picture clearly and is particularly adept at" _
& " inspection planning and analysis."
    Case "QQQBB"
lastSentence = "The ""QQQBB"" Inspector sees the big picture clearly and is extremely adept at" _
& " inspection planning and analysis."
    Case "QBBB"
lastSentence = "The ""QBBB"" Inspector sees the big picture very clearly and is adept at" _
& " inspection planning and analysis."
    Case "QQBBB"
lastSentence = "The ""QQBBB"" Inspector sees the big picture very clearly and is particularly adept at" _
& " inspection planning and analysis."
    Case "QQQBBB"
lastSentence = "The ""QQQBBB"" Inspector sees the big picture very clearly and is extremely adept at" _
& " inspection planning and analysis."
    Case "QBG"
lastSentence = "The ""QBG"" Inspector balances the big picture view with the task close at hand and is adept at" _
& " inspection planning and analysis."
    Case "QQBG"
lastSentence = "The ""QQBG"" Inspector balances the big picture view with the task close at hand and is particularly adept at" _
& " inspection planning and analysis."
    Case "QQQBG"
lastSentence = "The ""QQQBG"" Inspector balances the big picture view with the task close at hand and is extremely adept at" _
& " inspection planning and analysis."
    Case Else
lastSentence = "This Type needs to be added"
End Select

Else
description = "Financial Guardians are bound by fierce commitment, intense responsibility and deep loyalty.  They are sincere" _
& " and serious of purpose.   Their main mission is to conserve and guard financial assets and institutions against market" _
& " swings and fraud.   They are willing to work long, long hours doing all the thankless jobs that other types seem to ignore." _
& " They nobly work behind the scenes, allowing others the glory.   They are pessimistic and risk-averse by nature."

Select Case QandA
    Case "ABG"
lastSentence = "The ""ABG"" Financial Guardian balances the big picture view with the task close at hand and readily jumps to negative" _
& " conclusions about business downturns and the economy."
    Case "AABG"
lastSentence = "The ""AABG"" Financial Guardian balances the big picture view with the task close at hand and readily jumps to negative" _
& " conclusions about business downturns and the economy."
    Case "AAABG"
lastSentence = "The ""AAABG"" Financial Guardian balances the big picture view with the task close at hand and readily jumps to negative" _
& " conclusions about business downturns and the economy."
    Case "AB"
lastSentence = "The ""AB"" Financial Guardian sees the big picture and readily jumps to negative" _
& " conclusions about business downturns and the economy."
    Case "AAB"
lastSentence = "The ""AAB"" Financial Guardian sees the big picture and readily jumps to negative" _
& " conclusions about business downturns and the economy."
    Case "AAAB"
lastSentence = "The ""AAAB"" Financial Guardian sees the big picture and readily jumps to negative" _
& " conclusions about business downturns and the economy."
    Case "ABB"
lastSentence = "The ""ABB"" Financial Guardian sees the big picture clearly and readily jumps to negative" _
& " conclusions about business downturns and the economy."
    Case "AABB"
lastSentence = "The ""AABB"" Financial Guardian sees the big picture clearly and readily jumps to negative" _
& " conclusions about business downturns and the economy."
    Case "AAABB"
lastSentence = "The ""AAABB"" Financial Guardian sees the big picture clearly and readily jumps to negative" _
& " conclusions about business downturns and the economy."
    Case "ABBB"
lastSentence = "The ""ABBB"" Financial Guardian sees the big picture very clearly and readily jumps to negative" _
& " conclusions about business downturns and the economy."
    Case "AABBB"
lastSentence = "The ""AABBB"" Financial Guardian sees the big picture very clearly and readily jumps to negative" _
& " conclusions about business downturns and the economy."
    Case "AAABBB"
lastSentence = "The ""AAABBB"" Financial Guardian sees the big picture very clearly and readily jumps to negative" _
& " conclusions about business downturns and the economy."
    Case "AG"
lastSentence = "The ""AG"" Financial Guardian focuses on the task close at hand and readily jumps to negative" _
& " conclusions about business downturns and the economy."
    Case "AAG"
lastSentence = "The ""AAG"" Financial Guardian focuses on the task close at hand and readily jumps to negative" _
& " conclusions about business downturns and the economy."
    Case "AAAG"
lastSentence = "The ""AAAG"" Financial Guardian focuses on the task close at hand and readily jumps to negative" _
& " conclusions about business downturns and the economy."
    Case "AGG"
lastSentence = "The ""AGG"" Financial Guardian focuses very closely on the task at hand and readily jumps to negative" _
& " conclusions about business downturns and the economy."
    Case "AAGG"
lastSentence = "The ""AAGG"" Financial Guardian focuses very closely on the task at hand and readily jumps to negative" _
& " conclusions about business downturns and the economy."
    Case "AAAGG"
lastSentence = "The ""AAAGG"" Financial Guardian focuses very closely on the task at hand and readily jumps to negative" _
& " conclusions about business downturns and the economy."
    Case "AGGG"
lastSentence = "The ""AGGG"" Financial Guardian focuses extremely closely on the task at hand and readily jumps to negative" _
& " conclusions about business downturns and the economy."
    Case "AAGGG"
lastSentence = "The ""AAGGG"" Financial Guardian focuses extremely closely on the task at hand and readily jumps to negative" _
& " conclusions about business downturns and the economy."
    Case "AAAGGG"
lastSentence = "The ""AAAGGG"" Financial Guardian focuses extremely closely on the task at hand and readily jumps to negative" _
& " conclusions about business downturns and the economy."
End Select

End If
description = description & "   " & lastSentence
ISTJParagraph = description


End Function

Function ESTJParagraph(Qfirst, QandA)

If Qfirst Then
description = "Supervisors are life's administrators.  They are cooperative with their superiors and carry out orders" _
& " without fail and to the letter.  They are very serious about enforcing rules and procedures." _
& "  They are grounded, organized, exacting, socially deft, gregarious, and always" _
& " appropriate.  They are usually pillars of their communities, and make loyal, hardworking," _
& " dependable and risk-averse managers."

Select Case QandA
    Case "QG"
lastSentence = "The ""QG"" Supervisor focuses on the task close at hand and is adept at logistical planning and analysis."
    Case "QQG"
lastSentence = "The ""QQG"" Supervisor focuses on the task close at hand and is very adept at logistical planning and analysis."
    Case "QQQG"
lastSentence = "The ""QQQG"" Supervisor focuses on the task close at hand and is extremely adept at logistical planning and analysis."
    Case "QGG"
lastSentence = "The ""QGG"" Supervisor focuses very closely on the task at hand and is adept at logistical planning and analysis."
    Case "QQGG"
lastSentence = "The ""QQGG"" Supervisor focuses very closely on the task at hand and is very adept at logistical planning and analysis."
    Case "QQQGG"
lastSentence = "The ""QQQGG"" Supervisor focuses very closely on the task at hand and is extremely adept at logistical planning and analysis."
    Case "QGGG"
lastSentence = "The ""QGGG"" Supervisor focuses extremely closely on the task at hand and is adept at logistical planning and analysis."
    Case "QQGGG"
lastSentence = "The ""QQGGG"" Supervisor focuses extremely closely on the task at hand and is very adept at logistical planning and analysis."
    Case "QQQGGG"
lastSentence = "The ""QQQGGG"" Supervisor focuses extremely closely on the task at hand and is extremely adept at logistical planning and analysis."
    Case "QBG"
lastSentence = "The ""QBG"" Supervisor balances the big picture view with the task close at hand and is adept at logistical planning and analysis."
    Case "QQBG"
lastSentence = "The ""QQBG"" Supervisor balances the big picture view with the task close at hand and is particularly adept at logistical planning and analysis."
    Case "QQQBG"
lastSentence = "The ""QQQBG"" Supervisor balances the big picture view with the task close at hand and is extremely adept at logistical planning and analysis."
    Case "QB"
lastSentence = "The ""QB"" Supervisor sees the big picture and is adept at logistical planning and analysis."
    Case "QQB"
lastSentence = "The ""QQB"" Supervisor sees the big picture and is particularly adept at logistical planning and analysis."
   Case "QQQB"
lastSentence = "The ""QQQB"" Supervisor sees the big picture and is extremely adept at logistical planning and analysis."
    Case "QBB"
lastSentence = "The ""QBB"" Supervisor sees the big picture clearly and is adept at logistical planning and analysis."
    Case "QQBB"
lastSentence = "The ""QQBB"" Supervisor sees the big picture clearly and is particularly adept at logistical planning and analysis."
   Case "QQQBB"
lastSentence = "The ""QQQBB"" Supervisor sees the big picture clearly and is extremely adept at logistical planning and analysis."
    Case "QBBB"
lastSentence = "The ""QBBB"" Supervisor sees the big picture very clearly and is adept at logistical planning and analysis."
    Case "QQBBB"
lastSentence = "The ""QQBBB"" Supervisor sees the big picture very clearly and is particularly adept at logistical planning and analysis."
   Case "QQQBBB"
lastSentence = "The ""QQQBBB"" Supervisor sees the big picture very clearly and is extremely adept at logistical planning and analysis."
    
    Case Else
lastSentence = "This Type needs to be added"
End Select

Else
description = "Administrators are life's facilitators.  They are cooperative and accepting" _
& " of their superiors and carry out orders without fail and to the letter.   They" _
& " are grounded, organized, exacting, socially deft," _
& " gregarious, and always appropriate.  They are usually pillars of their communities and are loyal," _
& " hardworking, dependable, risk-averse, and pessimistic."

Select Case QandA
    Case "ABG"
lastSentence = "The ""ABG"" Administrator balances the big picture view with the task close at hand" _
& " and worries incessantly over incomplete projects."
    Case "AABG"
lastSentence = "The ""AABG"" Administrator balances the big picture view with the task close at hand" _
& " and worries incessantly over incomplete projects."
    Case "AAABG"
lastSentence = "The ""AAABG"" Administrator balances the big picture view with the task close at hand" _
& " and worries incessantly over incomplete projects."
    Case "AB"
lastSentence = "The ""AB"" Administrator sees the big picture and worries incessantly over incomplete projects."
    Case "AAB"
lastSentence = "The ""AAB"" Administrator sees the big picture and worries incessantly over incomplete projects."
    Case "AAAB"
lastSentence = "The ""AAAB"" Administrator sees the big picture and worries incessantly over incomplete projects."
    Case "ABB"
lastSentence = "The ""ABB"" Administrator sees the big picture clearly and worries incessantly over incomplete projects."
    Case "AABB"
lastSentence = "The ""AABB"" Administrator sees the big picture clearly and worries incessantly over incomplete projects."
    Case "AAABB"
lastSentence = "The ""AAABB"" Administrator sees the big picture clearly and worries incessantly over incomplete projects."
   Case "ABBB"
lastSentence = "The ""ABBB"" Administrator sees the big picture very clearly and worries incessantly over incomplete projects."
    Case "AABBB"
lastSentence = "The ""AABBB"" Administrator sees the big picture very clearly and worries incessantly over incomplete projects."
    Case "AAABBB"
lastSentence = "The ""AAABBB"" Administrator sees the big picture very clearly and worries incessantly over incomplete projects."
    Case "AG"
lastSentence = "The ""AG"" Administrator focuses on the task close at hand and worries incessantly over incomplete projects."
    Case "AAG"
lastSentence = "The ""AAG"" Administrator focuses on the task close at hand and worries incessantly over incomplete projects."
    Case "AAAG"
lastSentence = "The ""AAAG"" Administrator focuses on the task close at hand and worries incessantly over incomplete projects."
    Case "AGG"
lastSentence = "The ""AGG"" Administrator focuses very closely on the task at hand and worries incessantly over incomplete projects."
    Case "AAGG"
lastSentence = "The ""AAGG"" Administrator focuses very closely on the task at hand and worries incessantly over incomplete projects."
    Case "AAAGG"
lastSentence = "The ""AAAGG"" Administrator focuses very closely on the task at hand and worries incessantly over incomplete projects."
    Case "AGGG"
lastSentence = "The ""AGGG"" Administrator focuses extremely closely on the task at hand and worries incessantly over incomplete projects."
    Case "AAGGG"
lastSentence = "The ""AAGGG"" Administrator focuses extremely closely on the task at hand and worries incessantly over incomplete projects."
    Case "AAAGGG"
lastSentence = "The ""AAAGGG"" Administrator focuses extremely closely on the task at hand and worries incessantly over incomplete projects."
    Case Else
lastSentence = "This Type needs to be added"
End Select

End If

description = description & "   " & lastSentence
ESTJParagraph = description
End Function

Function ISFJParagraph(Qfirst, QandA)

If Qfirst Then
description = "Sensitive Inspectors are law-abiding, ""superdependable"" workers who are considerate of the feelings" _
& " of their co-workers and clients.   They carefully and thoroughly examine products and accounts of an institution" _
& " without flourish, fanfare, or insensitivity to the human side of auditing.   They are very realistic, down-to-earth" _
& " and practical.   They are pessimistic and risk-averse by nature and are very traditional and conservative" _
& " in their viewpoints.   They are dedicated, conscientious, and highly organized workers." _

Select Case QandA
    Case "QBG"
lastSentence = "The ""QBG"" Sensitive Inspector balances the big picture view with the task close at hand" _
& " and is adept at balancing the human side of auditing with the analytical side."
    Case "QQBG"
lastSentence = "The ""QQBG"" Sensitive Inspector balances the big picture view with the task close at hand" _
& " and is particularly adept at balancing the human side of auditing with the analytical side."
    Case "QQQBG"
lastSentence = "The ""QQQBG"" Sensitive Inspector balances the big picture view with the task close at hand" _
& " and is extremely adept at balancing the human side of auditing with the analytical side."
    Case "QB"
lastSentence = "The ""QB"" Sensitive Inspector sees the big picture and is adept at balancing the human side of auditing with the analytical side."
    Case "QQB"
lastSentence = "The ""QQB"" Sensitive Inspector sees the big picture and is particularly adept at balancing the human side of auditing with the analytical side."
    Case "QQQB"
lastSentence = "The ""QQQB"" Sensitive Inspector sees the big picture and is extremely adept at balancing the human side of auditing with the analytical side."
    Case "QBB"
lastSentence = "The ""QBB"" Sensitive Inspector sees the big picture clearly and is adept at balancing the human side of auditing with the analytical side."
    Case "QQBB"
lastSentence = "The ""QQBB"" Sensitive Inspector sees the big picture clearly and is particularly adept at balancing the human side of auditing with the analytical side."
    Case "QQQBB"
lastSentence = "The ""QQQBB"" Sensitive Inspector sees the big picture clearly and is extremely adept at balancing the human side of auditing with the analytical side."
    Case "QBBB"
lastSentence = "The ""QBBB"" Sensitive Inspector sees the big picture very clearly and is adept at balancing the human side of auditing with the analytical side."
    Case "QQBBB"
lastSentence = "The ""QQBBB"" Sensitive Inspector sees the big picture very clearly and is particularly adept at balancing the human side of auditing with the analytical side."
    Case "QQQBBB"
lastSentence = "The ""QQQBBB"" Sensitive Inspector sees the big picture very clearly and is extremely adept at balancing the human side of auditing with the analytical side."
    Case "QG"
lastSentence = "The ""QG"" Sensitive Inspector focuses on the task close at hand and is adept at balancing the human side of auditing with the analytical side."
    Case "QQG"
lastSentence = "The ""QQG"" Sensitive Inspector focuses on the task close at hand and is particularly adept at balancing the human side of auditing with the analytical side."
    Case "QQQG"
lastSentence = "The ""QQQG"" Sensitive Inspector focuses on the task close at hand and is extremely adept at balancing the human side of auditing with the analytical side."
    Case "QGG"
lastSentence = "The ""QGG"" Sensitive Inspector focuses very closely on the task at hand and is adept at balancing the human side of auditing with the analytical side."
    Case "QQGG"
lastSentence = "The ""QQGG"" Sensitive Inspector focuses very closely on the task at hand and is particularly adept at balancing the human side of auditing with the analytical side."
    Case "QQQGG"
lastSentence = "The ""QQQGG"" Sensitive Inspector focuses very closely on the task at hand and is extremely adept at balancing the human side of auditing with the analytical side."
    Case "QGGG"
lastSentence = "The ""QGGG"" Sensitive Inspector focuses extremely closely on the task at hand and is adept at balancing the human side of auditing with the analytical side."
    Case "QQGGG"
lastSentence = "The ""QQGGG"" Sensitive Inspector focuses extremely closely on the task at hand and is particularly adept at balancing the human side of auditing with the analytical side."
    Case "QQQGGG"
lastSentence = "The ""QQQGGG"" Sensitive Inspector focuses extremely closely on the task at hand and is exceptionally adept at balancing the human side of auditing with the analytical side."
    Case Else
lastSentence = "This Type needs to be added"
End Select

Else
description = "Protectors are bound by fierce commitment, intense responsibility and deep loyalty.  They are sincere" _
& " and serious of purpose.  Their main mission is to guard others against life's perils and pitfalls.  They are" _
& " willing to work long, long hours doing all the thankless jobs that other types seem to ignore.  They nobly" _
& " work behind the scenes, allowing others the glory.  They are pessimistic and risk-averse by nature." _

Select Case QandA
    Case "ABG"
lastSentence = "The ""ABG"" Protector balances the big picture view with the task close at hand and readily jumps to negative conclusions about people."
    Case "AABG"
lastSentence = "The ""AABG"" Protector balances the big picture view with the task close at hand and readily jumps to negative conclusions about people."
    Case "AAABG"
lastSentence = "The ""AAABG"" Protector balances the big picture view with the task close at hand and readily jumps to negative conclusions about people."
    Case "AB"
lastSentence = "The ""AB"" Protector sees the big picture and readily jumps to negative conclusions about people."
    Case "AAB"
lastSentence = "The ""AAB"" Protector sees the big picture and readily jumps to negative conclusions about people."
    Case "AAAB"
lastSentence = "The ""AAAB"" Protector sees the big picture and readily jumps to negative conclusions about people."
    Case "ABB"
lastSentence = "The ""ABB"" Protector sees the big picture clearly and readily jumps to negative conclusions about people."
    Case "AABB"
lastSentence = "The ""AABB"" Protector sees the big picture clearly and readily jumps to negative conclusions about people."
    Case "AAABB"
lastSentence = "The ""AAABB"" Protector sees the big picture clearly and readily jumps to negative conclusions about people."
    Case "ABBB"
lastSentence = "The ""ABBB"" Protector sees the big picture very clearly and readily jumps to negative conclusions about people."
    Case "AABBB"
lastSentence = "The ""AABBB"" Protector sees the big picture very clearly and readily jumps to negative conclusions about people."
    Case "AAABBB"
lastSentence = "The ""AAABBB"" Protector sees the big picture very clearly and readily jumps to negative conclusions about people."
    Case "AG"
lastSentence = "The ""AG"" Protector focuses on the task close at hand and readily jumps to negative conclusions about people."
    Case "AAG"
lastSentence = "The ""AAG"" Protector focuses on the task close at hand and readily jumps to negative conclusions about people."
    Case "AAAG"
lastSentence = "The ""AAAG"" Protector focuses on the task close at hand and readily jumps to negative conclusions about people."
    Case "AGG"
lastSentence = "The ""AGG"" Protector focuses very closely on the task at hand and readily jumps to negative conclusions about people."
    Case "AAGG"
lastSentence = "The ""AAGG"" Protector focuses very closely on the task at hand and readily jumps to negative conclusions about people."
    Case "AAAGG"
lastSentence = "The ""AAAGG"" Protector focuses very closely on the task at hand and readily jumps to negative conclusions about people."
    Case "AGGG"
lastSentence = "The ""AGGG"" Protector focuses extremely closely on the task at hand and readily jumps to negative conclusions about people."
    Case "AAGGG"
lastSentence = "The ""AAGGG"" Protector focuses extremely closely on the task at hand and readily jumps to negative conclusions about people."
    Case "AAAGGG"
lastSentence = "The ""AAAGGG"" Protector focuses extremely closely on the task at hand and readily jumps to negative conclusions about people."
    Case Else
lastSentence = "This Type needs to be added"
End Select

End If

description = description & "   " & lastSentence
ISFJParagraph = description
End Function

Function ISFPParagraph(Qfirst, QandA)

If Qfirst Then
description = "Sensitive Fire Fighters are reserved and aloof, but are quick to respond to the emergency needs of others." _
& "   They are ready to try anything once.   They thrive on excitement and risk-taking.   They are masterful operators of tools," _
& " equipment, machines, weapons, and instruments of all kinds.   Sensitive Fire Fighters are fearless in action and very impulsive when" _
& " people are at risk.   They prefer to follow their own lead and do not want to be subject to any rules or laws."

Select Case QandA
    Case "QBG"
lastSentence = "The ""QBG"" Sensitive Fire Fighter balances the big picture view with the task close at hand and is adept at diagnosing emergency situations with people."
    Case "QQBG"
lastSentence = "The ""QQBG"" Sensitive Fire Fighter balances the big picture view with the task close at hand and is particularly adept at diagnosing emergency situations with people."
    Case "QQQBG"
lastSentence = "The ""QQQBG"" Sensitive Fire Fighter balances the big picture view with the task close at hand and is extremely adept at diagnosing emergency situations with people."
    Case "QB"
lastSentence = "The ""QB"" Sensitive Fire Fighter sees the big picture and is adept at diagnosing emergency situations with people."
    Case "QQB"
lastSentence = "The ""QQB"" Sensitive Fire Fighter sees the big picture and is particularly adept at diagnosing emergency situations with people."
    Case "QQQB"
lastSentence = "The ""QQQB"" Sensitive Fire Fighter sees the big picture and is extremely adept at diagnosing emergency situations with people."
    Case "QBB"
lastSentence = "The ""QBB"" Sensitive Fire Fighter sees the big picture clearly and is adept at diagnosing emergency situations with people."
    Case "QQBB"
lastSentence = "The ""QQBB"" Sensitive Fire Fighter sees the big picture clearly and is particularly adept at diagnosing emergency situations with people."
    Case "QQQBB"
lastSentence = "The ""QQQBB"" Sensitive Fire Fighter sees the big picture clearly and is extremely adept at diagnosing emergency situations with people."
    Case "QBBB"
lastSentence = "The ""QBBB"" Sensitive Fire Fighter sees the big picture very clearly and is adept at diagnosing emergency situations with people."
    Case "QQBBB"
lastSentence = "The ""QQBBB"" Sensitive Fire Fighter sees the big picture very clearly and is particularly adept at diagnosing emergency situations with people."
    Case "QQQBBB"
lastSentence = "The ""QQQBBB"" Sensitive Fire Fighter sees the big picture very clearly and is extremely adept at diagnosing emergency situations with people."
    Case "QG"
lastSentence = "The ""QG"" Sensitive Fire Fighter focuses on the task close at hand and is adept at diagnosing emergency situations with people."
    Case "QQG"
lastSentence = "The ""QQG"" Sensitive Fire Fighter focuses on the task close at hand and is particularly adept at diagnosing emergency situations with people."
    Case "QQQG"
lastSentence = "The ""QQQG"" Sensitive Fire Fighter focuses on the task close at hand and is extremely adept at diagnosing emergency situations with people."
    Case "QGG"
lastSentence = "The ""QGG"" Sensitive Fire Fighter focuses very closely on the task at hand and is adept at diagnosing emergency situations with people."
    Case "QQGG"
lastSentence = "The ""QQGG"" Sensitive Fire Fighter focuses very closely on the task at hand and is particularly adept at diagnosing emergency situations with people."
    Case "QQQGG"
lastSentence = "The ""QQQGG"" Sensitive Fire Fighter focuses very closely on the task at hand and is extremely adept at diagnosing emergency situations with people."
    Case "QGGG"
lastSentence = "The ""QGGG"" Sensitive Fire Fighter focuses extremely closely on the task at hand and is adept at diagnosing emergency situations with people."
    Case "QQGGG"
lastSentence = "The ""QQGGG"" Sensitive Fire Fighter focuses extremely closely on the task at hand and is particularly adept at diagnosing emergency situations with people."
    Case "QQQGGG"
lastSentence = "The ""QQQGGG"" Sensitive Fire Fighter focuses extremely closely on the task at hand and is exceptionally adept at diagnosing emergency situations with people."
End Select

Else
description = "Artisans are gentle, soft-spoken, modest, easygoing, relaxed and non-competitive.   Of all the types," _
& " they are most in touch with their five senses and have a natural talent for the fine arts.   They are extremely sensitive" _
& " and nurturing of others and strive for harmony.   They have little desire to control or influence others, but will go to great" _
& " lengths to please people.   They are the kindest of all types."

Select Case QandA
    Case "ABG"
lastSentence = "The ""ABG"" Artisan balances the big picture view with the task close at hand and readily accepts the differences of others."
    Case "AABG"
lastSentence = "The ""AABG"" Artisan balances the big picture view with the task close at hand and readily accepts the differences of others."
    Case "AAABG"
lastSentence = "The ""AAABG"" Artisan balances the big picture view with the task close at hand and readily accepts the differences of others."
    Case "AB"
lastSentence = "The ""AB"" Artisan sees the big picture and readily accepts the differences of others."
    Case "AAB"
lastSentence = "The ""AAB"" Artisan sees the big picture and readily accepts the differences of others."
    Case "AAAB"
lastSentence = "The ""AAAB"" Artisan sees the big picture and readily accepts the differences of others."
    Case "ABB"
lastSentence = "The ""ABB"" Artisan sees the big picture clearly and readily accepts the differences of others."
    Case "AABB"
lastSentence = "The ""AABB"" Artisan sees the big picture clearly and readily accepts the differences of others."
    Case "AAABB"
lastSentence = "The ""AAABB"" Artisan sees the big picture clearly and readily accepts the differences of others."
    Case "ABBB"
lastSentence = "The ""ABBB"" Artisan sees the big picture very clearly and readily accepts the differences of others."
    Case "AABBB"
lastSentence = "The ""AABBB"" Artisan sees the big picture very clearly and readily accepts the differences of others."
    Case "AAABBB"
lastSentence = "The ""AAABBB"" Artisan sees the big picture very clearly and readily accepts the differences of others."
    Case "AG"
lastSentence = "The ""AG"" Artisan focuses on the task close at hand and readily accepts the differences of others."
    Case "AAG"
lastSentence = "The ""AAG"" Artisan focuses on the task close at hand and readily accepts the differences of others."
    Case "AAAG"
lastSentence = "The ""AAAG"" Artisan focuses on the task close at hand and readily accepts the differences of others."
    Case "AGG"
lastSentence = "The ""AGG"" Artisan focuses very closely on the task at hand and readily accepts the differences of others."
    Case "AAGG"
lastSentence = "The ""AAGG"" Artisan focuses very closely on the task at hand and readily accepts the differences of others."
    Case "AAAGG"
lastSentence = "The ""AAAGG"" Artisan focuses very closely on the task at hand and readily accepts the differences of others."
    Case "AGGG"
lastSentence = "The ""AGGG"" Artisan focuses extremely closely on the task at hand and readily accepts the differences of others."
    Case "AAGGG"
lastSentence = "The ""AAGGG"" Artisan focuses extremely closely on the task at hand and readily accepts the differences of others."
    Case "AAAGGG"
lastSentence = "The ""AAAGGG"" Artisan focuses extremely closely on the task at hand and readily accepts the differences of others."
End Select

End If

description = description & "   " & lastSentence
ISFPParagraph = description
End Function

Function ESFPParagraph(Qfirst, QandA)

If Qfirst Then
description = "Passionate Promoters are socially gregarious, fun-loving, free-spirited and energetic.   Of all the types," _
& " they are easily the most persuasive.   They live in the moment and close to the edge.   They are usually willing to do" _
& " what it takes to achieve their goals.   Passionate Promoters are excellent trouble-shooters and very tough, but personable negotiators." _
& "  They are sharp entrepreneurs, able to quickly swing deals and kick-start enterprises." _

Select Case QandA
    Case "QBG"
lastSentence = "The ""QBG"" Passionate Promoter balances the big picture view with the task close at hand and is adept at analyzing and manipulating people."
    Case "QQBG"
lastSentence = "The ""QQBG"" Passionate Promoter balances the big picture view with the task close at hand and is particularly adept at analyzing and manipulating people."
    Case "QQQBG"
lastSentence = "The ""QQQBG"" Passionate Promoter balances the big picture view with the task close at hand and is extremely adept at analyzing and manipulating people."
    Case "QB"
lastSentence = "The ""QB"" Passionate Promoter sees the big picture and is adept at analyzing and manipulating people."
    Case "QQB"
lastSentence = "The ""QQB"" Passionate Promoter sees the big picture and is particularly adept at analyzing and manipulating people."
    Case "QQQB"
lastSentence = "The ""QQQB"" Passionate Promoter sees the big picture and is extremely adept at analyzing and manipulating people."
    Case "QBB"
lastSentence = "The ""QBB"" Passionate Promoter sees the big picture clearly and is adept at analyzing and manipulating people."
    Case "QQBB"
lastSentence = "The ""QQBB"" Passionate Promoter sees the big picture clearly and is particularly adept at analyzing and manipulating people."
    Case "QQQBB"
lastSentence = "The ""QQQBB"" Passionate Promoter sees the big picture clearly and is extremely adept at analyzing and manipulating people."
    Case "QBBB"
lastSentence = "The ""QBBB"" Passionate Promoter sees the big picture very clearly and is adept at analyzing and manipulating people."
    Case "QQBBB"
lastSentence = "The ""QQBBB"" Passionate Promoter sees the big picture very clearly and is particularly adept at analyzing and manipulating people."
    Case "QQQBBB"
lastSentence = "The ""QQQBBB"" Passionate Promoter sees the big picture very clearly and is extremely adept at analyzing and manipulating people."
    Case "QG"
lastSentence = "The ""QG"" Passionate Promoter focuses on the task close at hand and is adept at analyzing" _
& " and manipulating people."
    Case "QQG"
lastSentence = "The ""QQG"" Passionate Promoter focuses on the task close at hand and is particularly adept at analyzing" _
& " and manipulating people."
    Case "QQQG"
lastSentence = "The ""QQQG"" Passionate Promoter focuses on the task close at hand and is extremely adept at analyzing" _
& " and manipulating people."
    Case "QGG"
lastSentence = "The ""QGG"" Passionate Promoter focuses very closely on the task at hand and is adept at analyzing" _
& " and manipulating people."
    Case "QQGG"
lastSentence = "The ""QQGG"" Passionate Promoter focuses very closely on the task at hand and is particularly adept at analyzing" _
& " and manipulating people."
    Case "QQQGG"
lastSentence = "The ""QQQGG"" Passionate Promoter focuses very closely on the task at hand and is extremely adept at analyzing" _
& " and manipulating people."
    Case "QGGG"
lastSentence = "The ""QGGG"" Passionate Promoter focuses extremely closely on the task at hand and is adept at analyzing" _
& " and manipulating people."
    Case "QQGGG"
lastSentence = "The ""QQGGG"" Passionate Promoter focuses extremely closely on the task at hand and is particularly adept at analyzing" _
& " and manipulating people."
    Case "QQQGGG"
lastSentence = "The ""QQQGGG"" Passionate Promoter focuses extremely closely on the task at hand and is extremely adept at analyzing" _
& " and manipulating people."
    Case Else
lastSentence = "This Type needs to be added"
End Select

Else
description = "Performers are warm, friendly, and have an easy-going nature.   They enjoy people and doing nice things for them." _
& "  They love the excitement of playing to an audience and quickly become the center of attention.   They are incurably" _
& " optimistic.   Performers are the most in touch with the here-and-now.   They have a low tolerance for procedures," _
& " routines or anything else that stands in the way of immediate gratification." _

Select Case QandA
    Case "ABG"
lastSentence = "The ""ABG"" Performer balances the big picture view with the task close at hand and is adept at" _
& " lifting the spirits of others."
    Case "AABG"
lastSentence = "The ""AABG"" Performer balances the big picture view with the task close at hand and is adept at" _
& " lifting the spirits of others."
    Case "AAABG"
lastSentence = "The ""AAABG"" Performer balances the big picture view with the task close at hand and is adept at" _
& " lifting the spirits of others."
    Case "AB"
lastSentence = "The ""AB"" Performer sees the big picture and is adept at lifting the spirits of others."
    Case "AAB"
lastSentence = "The ""AAB"" Performer sees the big picture and is adept at lifting the spirits of others."
    Case "AAAB"
lastSentence = "The ""AAAB"" Performer sees the big picture and is adept at lifting the spirits of others."
    Case "ABB"
lastSentence = "The ""ABB"" Performer sees the big picture clearly and is adept at lifting the spirits of others."
    Case "AABB"
lastSentence = "The ""AABB"" Performer sees the big picture clearly and is adept at lifting the spirits of others."
    Case "AAABB"
lastSentence = "The ""AAABB"" Performer sees the big picture clearly and is adept at lifting the spirits of others."
    Case "ABBB"
lastSentence = "The ""ABBB"" Performer sees the big picture very clearly and is adept at lifting the spirits of others."
    Case "AABBB"
lastSentence = "The ""AABBB"" Performer sees the big picture very clearly and is adept at lifting the spirits of others."
    Case "AAABBB"
lastSentence = "The ""AAABBB"" Performer sees the big picture very clearly and is adept at lifting the spirits of others."
    Case "AG"
lastSentence = "The ""AG"" Performer focuses on the task close at hand and is adept at lifting the spirits of others."
    Case "AAG"
lastSentence = "The ""AAG"" Performer focuses on the task close at hand and is adept at lifting the spirits of others."
    Case "AAAG"
lastSentence = "The ""AAAG"" Performer focuses on the task close at hand and is adept at lifting the spirits of others."
    Case "AGG"
lastSentence = "The ""AGG"" Performer focuses very closely on the task at hand and is adept at lifting the spirits of others."
    Case "AAGG"
lastSentence = "The ""AAGG"" Performer focuses very closely on the task at hand and is adept at lifting the spirits of others."
    Case "AAAGG"
lastSentence = "The ""AAAGG"" Performer focuses very closely on the task at hand and is adept at lifting the spirits of others."
    Case "AGGG"
lastSentence = "The ""AGGG"" Performer focuses extremely closely on the task at hand and is adept at lifting the spirits of others."
    Case "AAGGG"
lastSentence = "The ""AAGGG"" Performer focuses extremely closely on the task at hand and is adept at lifting the spirits of others."
    Case "AAAGGG"
lastSentence = "The ""AAAGGG"" Performer focuses extremely closely on the task at hand and is adept at lifting the spirits of others."
    Case Else
lastSentence = "This Type needs to be added"
End Select

End If

description = description & "   " & lastSentence
ESFPParagraph = description
End Function

Function ESFJParagraph(Qfirst, QandA)

If Qfirst Then
description = "Passionate Supervisors are life's motivators.  They are cooperative with their superiors" _
& " and carry out orders without fail and to the letter.  They are serious about enforcing rules and procedures." _
& "  They are grounded, organized, exacting, socially deft, gregarious and always appropriate.  They are usually" _
& " pillars of their community.  They make loyal, hardworking, and dependable managers.   They are risk-averse by nature."

Select Case QandA
    Case "QBG"
lastSentence = "The ""QBG"" Passionate Supervisor balances the big picture view with the task close at hand and" _
& " is adept at motivating people."
    Case "QQBG"
lastSentence = "The ""QQBG"" Passionate Supervisor balances the big picture view with the task close at hand and" _
& " is particularly adept at motivating people."
    Case "QQQBG"
lastSentence = "The ""QQQBG"" Passionate Supervisor balances the big picture view with the task close at hand and" _
& " is extremely adept at motivating people."
    Case "QB"
lastSentence = "The ""QB"" Passionate Supervisor sees the big picture and is adept at motivating people."
    Case "QQB"
lastSentence = "The ""QQB"" Passionate Supervisor sees the big picture and is particularly adept at motivating people."
    Case "QQQB"
lastSentence = "The ""QQQB"" Passionate Supervisor sees the big picture and is extremely adept at motivating people."
    Case "QBB"
lastSentence = "The ""QBB"" Passionate Supervisor sees the big picture clearly and is adept at motivating people."
    Case "QQBB"
lastSentence = "The ""QQBB"" Passionate Supervisor sees the big picture clearly and is particularly adept at motivating people."
    Case "QQQBB"
lastSentence = "The ""QQQBB"" Passionate Supervisor sees the big picture clearly and is extremely adept at motivating people."
    Case "QBBB"
lastSentence = "The ""QBBB"" Passionate Supervisor sees the big picture very clearly and is adept at motivating people."
    Case "QQBBB"
lastSentence = "The ""QQBBB"" Passionate Supervisor sees the big picture very clearly and is particularly adept at motivating people."
    Case "QQQBBB"
lastSentence = "The ""QQQBBB"" Passionate Supervisor sees the big picture very clearly and is extremely adept at motivating people."
    Case "QG"
lastSentence = "The ""QG"" Passionate Supervisor focuses on the task close at hand and is adept at motivating people."
    Case "QQG"
lastSentence = "The ""QQG"" Passionate Supervisor focuses on the task close at hand and is particularly adept at motivating people."
    Case "QQQG"
lastSentence = "The ""QQQG"" Passionate Supervisor focuses on the task close at hand and is extremely adept at motivating people."
    Case "QGG"
lastSentence = "The ""QGG"" Passionate Supervisor focuses very closely on the task at hand and is adept at motivating people."
    Case "QQGG"
lastSentence = "The ""QQGG"" Passionate Supervisor focuses very closely on the task at hand and is particularly adept at motivating people."
    Case "QQQGG"
lastSentence = "The ""QQQGG"" Passionate Supervisor focuses very closely on the task at hand and is extremely adept at motivating people."
    Case "QGGG"
lastSentence = "The ""QGGG"" Passionate Supervisor focuses extremely closely on the task at hand and is adept at motivating people."
    Case "QQGGG"
lastSentence = "The ""QQGGG"" Passionate Supervisor focuses extremely closely on the task at hand and is particularly adept at motivating people."
    Case "QQQGGG"
lastSentence = "The ""QQQGGG"" Passionate Supervisor focuses exceptionally closely on the task at hand and is extremely adept at motivating people."

    Case Else
lastSentence = "This Type needs to be added"
End Select

Else
description = "Providers are the hosts and hostesses of the world.  They are the most sociable of all the" _
& " types and are great nurturers of established institutions.  They make sure that the needs of others are" _
& " met, that traditions are supported and that social functions are a success.  They are without peer as" _
& " masters of ceremonies.  They are personally loyal, highly cooperative, gracious, and devoted to their" _
& " employers." _

Select Case QandA
    Case "ABG"
lastSentence = "The ""ABG"" Provider balances the big picture view with the task close at hand and readily accepts the differences of others."
    Case "AABG"
lastSentence = "The ""AABG"" Provider balances the big picture view with the task close at hand and readily accepts the differences of others."
    Case "AAABG"
lastSentence = "The ""AAABG"" Provider balances the big picture view with the task close at hand and readily accepts the differences of others."
    Case "AB"
lastSentence = "The ""AB"" Provider sees the big picture and readily accepts the differences of others."
    Case "AAB"
lastSentence = "The ""AAB"" Provider sees the big picture and readily accepts the differences of others."
    Case "AAAB"
lastSentence = "The ""AAAB"" Provider sees the big picture and readily accepts the differences of others."
    Case "ABB"
lastSentence = "The ""ABB"" Provider sees the big picture clearly and readily accepts the differences of others."
    Case "AABB"
lastSentence = "The ""AABB"" Provider sees the big picture clearly and readily accepts the differences of others."
    Case "AAABB"
lastSentence = "The ""AAABB"" Provider sees the big picture clearly and readily accepts the differences of others."
    Case "ABBB"
lastSentence = "The ""ABBB"" Provider sees the big picture very clearly and readily accepts the differences of others."
    Case "AABBB"
lastSentence = "The ""AABBB"" Provider sees the big picture very clearly and readily accepts the differences of others."
    Case "AAABBB"
lastSentence = "The ""AAABBB"" Provider sees the big picture very clearly and readily accepts the differences of others."
    Case "AG"
lastSentence = "The ""AG"" Provider focuses on the task close at hand and readily accepts the differences of others."
    Case "AAG"
lastSentence = "The ""AAG"" Provider focuses on the task close at hand and readily accepts the differences of others."
    Case "AAAG"
lastSentence = "The ""AAAG"" Provider focuses on the task close at hand and readily accepts the differences of others."
    Case "AGG"
lastSentence = "The ""AGG"" Provider focuses very closely on the task at hand and readily accepts the differences of others."
    Case "AAGG"
lastSentence = "The ""AAGG"" Provider focuses very closely on the task at hand and readily accepts the differences of others."
    Case "AAAGG"
lastSentence = "The ""AAAGG"" Provider focuses very closely on the task at hand and readily accepts the differences of others."
    Case "AGGG"
lastSentence = "The ""AGGG"" Provider focuses extremely closely on the task at hand and readily accepts the differences of others."
    Case "AAGGG"
lastSentence = "The ""AAGGG"" Provider focuses extremely closely on the task at hand and readily accepts the differences of others."
    Case "AAAGGG"
lastSentence = "The ""AAAGGG"" Provider focuses extremely closely on the task at hand and readily accepts the differences of others."
    Case Else
lastSentence = "This Type needs to be added"
End Select

End If

description = description & "   " & lastSentence
ESFJParagraph = description
End Function

Function INFJParagraph(Qfirst, QandA)

If Qfirst Then
description = "Sensitive Masterminds are excellent at planning operations that are sensitive to the human factor." _
& "  They are way above the other types in ""if-then"" contingency planning.   They always have a Plan A," _
& " but are prepared to switch to Plan B, C, or D.  Being independent and highly self-confident, they are" _
& " very open-minded and will entertain any idea that might prove useful.   They tend to verbalize the" _
& " positive and avoid negative comments." _

Select Case QandA
    Case "QBG"
lastSentence = "The ""QBG"" Sensitive Mastermind balances the big picture view with the task close at hand and" _
& " is adept at analyzing both the logic and human sides of an operation."
    Case "QQBG"
lastSentence = "The ""QQBG"" Sensitive Mastermind balances the big picture view with the task close at hand and" _
& " is particularly adept at analyzing both the logic and human sides of an operation."
    Case "QQQBG"
lastSentence = "The ""QQQBG"" Sensitive Mastermind balances the big picture view with the task close at hand and" _
& " is extremely adept at analyzing both the logic and human sides of an operation."
    Case "QB"
lastSentence = "The ""QB"" Sensitive Mastermind sees the big picture and is adept" _
& " at analyzing both the logic and human sides of an operation."
    Case "QQB"
lastSentence = "The ""QQB"" Sensitive Mastermind sees the big picture and is particularly adept" _
& " at analyzing both the logic and human sides of an operation."
    Case "QQQB"
lastSentence = "The ""QQQB"" Sensitive Mastermind sees the big picture and is extremely adept" _
& " at analyzing both the logic and human sides of an operation."
    Case "QBB"
lastSentence = "The ""QBB"" Sensitive Mastermind sees the big picture clearly and is adept" _
& " at analyzing both the logic and human sides of an operation."
    Case "QQBB"
lastSentence = "The ""QQBB"" Sensitive Mastermind sees the big picture clearly and is particularly adept" _
& " at analyzing both the logic and human sides of an operation."
    Case "QQQBB"
lastSentence = "The ""QQQBB"" Sensitive Mastermind sees the big picture clearly and is extremely adept" _
& " at analyzing both the logic and human sides of an operation."
    Case "QBBB"
lastSentence = "The ""QBBB"" Sensitive Mastermind sees the big picture very clearly and is adept" _
& " at analyzing both the logic and human sides of an operation."
    Case "QQBBB"
lastSentence = "The ""QQBBB"" Sensitive Mastermind sees the big picture very clearly and is particularly adept" _
& " at analyzing both the logic and human sides of an operation."
    Case "QQQBBB"
lastSentence = "The ""QQQBBB"" Sensitive Mastermind sees the big picture very clearly and is extremely adept" _
& " at analyzing both the logic and human sides of an operation."
    Case "QG"
lastSentence = "The ""QG"" Sensitive Mastermind focuses on the task close at hand and is adept at analyzing" _
& " both the logic and human sides of an operation."
    Case "QQG"
lastSentence = "The ""QQG"" Sensitive Mastermind focuses on the task close at hand and is particularly adept at analyzing" _
& " both the logic and human sides of an operation."
    Case "QQQG"
lastSentence = "The ""QQQG"" Sensitive Mastermind focuses on the task close at hand and is extremely adept at analyzing" _
& " both the logic and human sides of an operation."
    Case "QGG"
lastSentence = "The ""QGG"" Sensitive Mastermind focuses very closely on the task at hand and is adept at analyzing" _
& " both the logic and human sides of an operation."
    Case "QQGG"
lastSentence = "The ""QQGG"" Sensitive Mastermind focuses very closely on the task at hand and is particularly adept at analyzing" _
& " both the logic and human sides of an operation."
    Case "QQQGG"
lastSentence = "The ""QQQGG"" Sensitive Mastermind focuses very closely on the task at hand and is extremely adept at analyzing" _
& " both the logic and human sides of an operation."
    Case "QGGG"
lastSentence = "The ""QGGG"" Sensitive Mastermind focuses extremely closely on the task at hand and is adept at analyzing" _
& " both the logic and human sides of an operation."
    Case "QQGGG"
lastSentence = "The ""QQGGG"" Sensitive Mastermind focuses extremely closely on the task at hand and is particularly adept at analyzing" _
& " both the logic and human sides of an operation."
    Case "QQQGGG"
lastSentence = "The ""QQQGGG"" Sensitive Mastermind focuses extremely closely on the task at hand and is exceptionally adept at analyzing" _
& " both the logic and human sides of an operation."
    Case Else
lastSentence = "This Type needs to be added"
End Select

Else
description = "Counselors are gentle, compassionate, compliant, reserved and accepting.   However, when" _
& " they are committed to a cause or an ideal, they can be extremely stubborn.   Counselors have a strong" _
& " desire to contribute to the welfare of others.   Their genuine caring and concern often inspires" _
& " others.  They value staff harmony and make every effort to help an organization run smoothly and" _
& " pleasantly." _

Select Case QandA
    Case "ABG"
lastSentence = "The ""ABG"" Counselor balances the big picture view with the task close at hand and" _
& " readily accepts the differences of others."
    Case "AABG"
lastSentence = "The ""AABG"" Counselor balances the big picture view with the task close at hand and" _
& " readily accepts the differences of others."
    Case "AAABG"
lastSentence = "The ""AAABG"" Counselor balances the big picture view with the task close at hand and" _
& " readily accepts the differences of others."
    Case "AB"
lastSentence = "The ""AB"" Counselor sees the big picture and readily accepts the differences of others."
    Case "AAB"
lastSentence = "The ""AAB"" Counselor sees the big picture and readily accepts the differences of others."
    Case "AAAB"
lastSentence = "The ""AAAB"" Counselor sees the big picture and readily accepts the differences of others."
    Case "ABB"
lastSentence = "The ""ABB"" Counselor sees the big picture clearly and readily accepts the differences of others."
    Case "AABB"
lastSentence = "The ""AABB"" Counselor sees the big picture clearly and readily accepts the differences of others."
    Case "AAABB"
lastSentence = "The ""AAABB"" Counselor sees the big picture clearly and readily accepts the differences of others."
    Case "ABBB"
lastSentence = "The ""ABBB"" Counselor sees the big picture very clearly and readily accepts the differences of others."
    Case "AABBB"
lastSentence = "The ""AABBB"" Counselor sees the big picture very clearly and readily accepts the differences of others."
    Case "AAABBB"
lastSentence = "The ""AAABBB"" Counselor sees the big picture very clearly and readily accepts the differences of others."
    Case "AG"
lastSentence = "The ""AG"" Counselor focuses on the task close at hand and readily accepts the differences of others."
    Case "AAG"
lastSentence = "The ""AAG"" Counselor focuses on the task close at hand and readily accepts the differences of others."
    Case "AAAG"
lastSentence = "The ""AAAG"" Counselor focuses on the task close at hand and readily accepts the differences of others."
    Case "AGG"
lastSentence = "The ""AGG"" Counselor focuses very closely on the task at hand and readily accepts the differences of others."
    Case "AAGG"
lastSentence = "The ""AAGG"" Counselor focuses very closely on the task at hand and readily accepts the differences of others."
    Case "AAAGG"
lastSentence = "The ""AAAGG"" Counselor focuses very closely on the task at hand and readily accepts the differences of others."
    Case "AGGG"
lastSentence = "The ""AGGG"" Counselor focuses extremely closely on the task at hand and readily accepts the differences of others."
    Case "AAGGG"
lastSentence = "The ""AAGGG"" Counselor focuses extremely closely on the task at hand and readily accepts the differences of others."
    Case "AAAGGG"
lastSentence = "The ""AAAGGG"" Counselor focuses extremely closely on the task at hand and readily accepts the differences of others."
    Case Else
lastSentence = "This Type needs to be added"
End Select

End If

description = description & "   " & lastSentence
INFJParagraph = description
End Function

Function INFPParagraph(Qfirst, QandA)

If Qfirst Then
description = "Human Scientists are the architects of all kinds of theoretical systems that relate to" _
& " people.   People exist to be analyzed, understood and explained.   Curiosity about people and what" _
& " makes them tick are their driving force.   Human Scientists are ruthless pragmatists about people." _
& "  They exhibit great precision in thought and speech.   They see inconsistencies instantly and are" _
& " devastating in debate." _

Select Case QandA
    Case "QBG"
lastSentence = "The ""QBG"" Human Scientist balances the big picture view with the task close at hand" _
& " and is adept at analyzing and balancing both the logic and human sides of a problem."
    Case "QQBG"
lastSentence = "The ""QQBG"" Human Scientist balances the big picture view with the task close at hand" _
& " and is particularly adept at analyzing and balancing both the logic and human sides of a problem."
    Case "QQQBG"
lastSentence = "The ""QQQBG"" Human Scientist balances the big picture view with the task close at hand" _
& " and is extremely adept at analyzing and balancing both the logic and human sides of a problem."
    Case "QB"
lastSentence = "The ""QB"" Human Scientist sees the big picture and is adept at analyzing and balancing" _
& " both the logic and human sides of a problem."
    Case "QQB"
lastSentence = "The ""QQB"" Human Scientist sees the big picture and is particularly adept at analyzing and balancing" _
& " both the logic and human sides of a problem."
    Case "QQQB"
lastSentence = "The ""QQQB"" Human Scientist sees the big picture and is extremely adept at analyzing and balancing" _
& " both the logic and human sides of a problem."
    Case "QBB"
lastSentence = "The ""QBB"" Human Scientist sees the big picture clearly and is adept at analyzing and balancing" _
& " both the logic and human sides of a problem."
    Case "QQBB"
lastSentence = "The ""QQBB"" Human Scientist sees the big picture clearly and is particularly adept at analyzing and balancing" _
& " both the logic and human sides of a problem."
    Case "QQQBB"
lastSentence = "The ""QQQBB"" Human Scientist sees the big picture clearly and is extremely adept at analyzing and balancing" _
& " both the logic and human sides of a problem."
    Case "QBBB"
lastSentence = "The ""QBBB"" Human Scientist sees the big picture very clearly and is adept at analyzing and balancing" _
& " both the logic and human sides of a problem."
    Case "QQBBB"
lastSentence = "The ""QQBBB"" Human Scientist sees the big picture very clearly and is particularly adept at analyzing and balancing" _
& " both the logic and human sides of a problem."
    Case "QQQBBB"
lastSentence = "The ""QQQBBB"" Human Scientist sees the big picture very clearly and is extremely adept at analyzing and balancing" _
& " both the logic and human sides of a problem."
    Case "QG"
lastSentence = "The ""QG"" Human Scientist focuses on the task close at hand and is adept at analyzing and balancing" _
& " both the logic and human sides of a problem."
    Case "QQG"
lastSentence = "The ""QQG"" Human Scientist focuses on the task close at hand and is particularly adept at analyzing and balancing" _
& " both the logic and human sides of a problem."
    Case "QQQG"
lastSentence = "The ""QQQG"" Human Scientist focuses on the task close at hand and is extremely adept at analyzing and balancing" _
& " both the logic and human sides of a problem."
    Case "QGG"
lastSentence = "The ""QGG"" Human Scientist focuses very closely on the task at hand and is adept at analyzing and balancing" _
& " both the logic and human sides of a problem."
    Case "QQGG"
lastSentence = "The ""QQGG"" Human Scientist focuses very closely on the task at hand and is particularly adept at analyzing and balancing" _
& " both the logic and human sides of a problem."
    Case "QQQGG"
lastSentence = "The ""QQQGG"" Human Scientist focuses very closely on the task at hand and is extremely adept at analyzing and balancing" _
& " both the logic and human sides of a problem."
    Case "QGGG"
lastSentence = "The ""QGGG"" Human Scientist focuses extremely closely on the task at hand and is adept at analyzing and balancing" _
& " both the logic and human sides of a problem."
    Case "QQGGG"
lastSentence = "The ""QQGGG"" Human Scientist focuses extremely closely on the task at hand and is particularly adept at analyzing and balancing" _
& " both the logic and human sides of a problem."
    Case "QQQGGG"
lastSentence = "The ""QQQGGG"" Human Scientist focuses extremely closely on the task at hand and is extremely adept at analyzing and balancing" _
& " both the logic and human sides of a problem."
    Case Else
lastSentence = "This Type needs to be added"
End Select

Else
description = "Healers are easygoing, flexible, tranquil, warm, gracious and adaptive.   Yet, when their" _
& " self-imposed codes are trampled on, they can become very aggressive and demanding.   They have a capacity" _
& " for spiritual caring that is much greater than the other types.   Idealists are willing to make" _
& " extraordinary sacrifices for someone or something they believe in.   They seek to heal conflicts and bring" _
& " unity." _

Select Case QandA
    Case "ABG"
lastSentence = "The ""ABG"" Healer balances the big picture view with the task close at hand and readily accepts the differences of others."
    Case "AABG"
lastSentence = "The ""AABG"" Healer balances the big picture view with the task close at hand and readily accepts the differences of others."
    Case "AAABG"
lastSentence = "The ""AAABG"" Healer balances the big picture view with the task close at hand and readily accepts the differences of others."
    Case "AB"
lastSentence = "The ""AB"" Healer sees the big picture and readily accepts the differences of others."
    Case "AAB"
lastSentence = "The ""AAB"" Healer sees the big picture and readily accepts the differences of others."
    Case "AAAB"
lastSentence = "The ""AAAB"" Healer sees the big picture and readily accepts the differences of others."
    Case "ABB"
lastSentence = "The ""ABB"" Healer sees the big picture clearly and readily accepts the differences of others."
    Case "AABB"
lastSentence = "The ""AABB"" Healer sees the big picture clearly and readily accepts the differences of others."
    Case "AAABB"
lastSentence = "The ""AAABB"" Healer sees the big picture clearly and readily accepts the differences of others."
    Case "ABBB"
lastSentence = "The ""ABBB"" Healer sees the big picture very clearly and readily accepts the differences of others."
    Case "AABBB"
lastSentence = "The ""AABBB"" Healer sees the big picture very clearly and readily accepts the differences of others."
    Case "AAABBB"
lastSentence = "The ""AAABBB"" Healer sees the big picture very clearly and readily accepts the differences of others."
    Case "AG"
lastSentence = "The ""AG"" Healer focuses on the task close at hand and readily accepts the differences of others."
    Case "AAG"
lastSentence = "The ""AAG"" Healer focuses on the task close at hand and readily accepts the differences of others."
    Case "AAAG"
lastSentence = "The ""AAAG"" Healer focuses on the task close at hand and readily accepts the differences of others."
    Case "AGG"
lastSentence = "The ""AGG"" Healer focuses very closely on the task at hand and readily accepts the differences of others."
    Case "AAGG"
lastSentence = "The ""AAGG"" Healer focuses very closely on the task at hand and readily accepts the differences of others."
    Case "AAAGG"
lastSentence = "The ""AAAGG"" Healer focuses very closely on the task at hand and readily accepts the differences of others."
    Case "AGGG"
lastSentence = "The ""AGGG"" Healer focuses extremely closely on the task at hand and readily accepts the differences of others."
    Case "AAGGG"
lastSentence = "The ""AAGGG"" Healer focuses extremely closely on the task at hand and readily accepts the differences of others."
    Case "AAAGGG"
lastSentence = "The ""AAAGGG"" Healer focuses extremely closely on the task at hand and readily accepts the differences of others."
 
    Case Else
lastSentence = "This Type needs to be added"
End Select

End If

description = description & "   " & lastSentence
INFPParagraph = description
End Function

Function ENFPParagraph(Qfirst, QandA)

If Qfirst Then
description = "Marketing Strategists have natural ingenuity for advertising and marketing.  They are" _
& " intensely curious and continually probe for solutions to marketing problems.  They are always" _
& " on the lookout for a better way.  They are confident in their inventiveness and display an" _
& " extraordinary talent for rising to the demands of even the most impossible situation.  Rather" _
& " than make a detailed plan, they count on their ability to solve problems as they arise." _

Select Case QandA
    Case "QBG"
lastSentence = "The ""QBG"" Marketing Strategist balances the big picture view with the task close" _
& " at hand and is adept at analyzing what motivates people."
    Case "QQBG"
lastSentence = "The ""QQBG"" Marketing Strategist balances the big picture view with the task close" _
& " at hand and is particularly adept at analyzing what motivates people."
    Case "QQQBG"
lastSentence = "The ""QQQBG"" Marketing Strategist balances the big picture view with the task close" _
& " at hand and is extremely adept at analyzing what motivates people."
    Case "QB"
lastSentence = "The ""QB"" Marketing Strategist sees the big picture and is adept at analyzing what" _
& " motivates people."
    Case "QQB"
lastSentence = "The ""QQB"" Marketing Strategist sees the big picture and is particularly adept at analyzing" _
& " what motivates people."
    Case "QQQB"
lastSentence = "The ""QQQB"" Marketing Strategist sees the big picture and is extremely adept at analyzing" _
& " what motivates people."
    Case "QBB"
lastSentence = "The ""QBB"" Marketing Strategist sees the big picture clearly and is adept at analyzing what" _
& " motivates people."
    Case "QQBB"
lastSentence = "The ""QQBB"" Marketing Strategist sees the big picture clearly and is particularly adept at analyzing" _
& " what motivates people."
    Case "QQQBB"
lastSentence = "The ""QQQBB"" Marketing Strategist sees the big picture clearly and is extremely adept at analyzing" _
& " what motivates people."
    Case "QBBB"
lastSentence = "The ""QBBB"" Marketing Strategist sees the big picture very clearly and is adept at analyzing what" _
& " motivates people."
    Case "QQBBB"
lastSentence = "The ""QQBBB"" Marketing Strategist sees the big picture very clearly and is particularly adept at analyzing" _
& " what motivates people."
    Case "QQQBBB"
lastSentence = "The ""QQQBBB"" Marketing Strategist sees the big picture very clearly and is extremely adept at analyzing" _
& " what motivates people."
    Case "QG"
lastSentence = "The ""QG"" Marketing Strategist focuses on the task close at hand and is" _
& " adept at analyzing what motivates people."
    Case "QQG"
lastSentence = "The ""QQG"" Marketing Strategist focuses on the task close at hand and is" _
& " particularly adept at analyzing what motivates people."
    Case "QQQG"
lastSentence = "The ""QQQG"" Marketing Strategist focuses on the task close at hand and is" _
& " extremely adept at analyzing what motivates people."
    Case "QGG"
lastSentence = "The ""QGG"" Marketing Strategist focuses very closely on the task at hand and is" _
& " adept at analyzing what motivates people."
    Case "QQGG"
lastSentence = "The ""QQGG"" Marketing Strategist focuses very closely on the task at hand and is" _
& " particularly adept at analyzing what motivates people."
    Case "QQQGG"
lastSentence = "The ""QQQGG"" Marketing Strategist focuses very closely on the task at hand and is" _
& " extremely adept at analyzing what motivates people."
    Case "QGGG"
lastSentence = "The ""QGGG"" Marketing Strategist focuses extremely closely on the task at hand and is" _
& " adept at analyzing what motivates people."
    Case "QQGGG"
lastSentence = "The ""QQGGG"" Marketing Strategist focuses extremely closely on the task at hand and is" _
& " particularly adept at analyzing what motivates people."
    Case "QQQGGG"
lastSentence = "The ""QQQGGG"" Marketing Strategist focuses extremely closely on the task at hand and is" _
& " extremely adept at analyzing what motivates people."
    Case Else
lastSentence = "This Type needs to be added"
End Select
Else

description = "Champions are dynamic, enthusiastic, highly skilled with people, and gregarious." _
& "  Their enthusiasm is boundless and often contagious, making them the most outgoing of all the" _
& " types.  They love being around people.   They are very affirming and complimentary.   They" _
& " enjoy telling jokes and stories.  They are naturally irreverent.  Their language is peppered" _
& " with humor and metaphors.  Champions are insatiably curious and quick to strike up conversations" _
& " with strangers, asking a lot of questions." _

Select Case QandA
    Case "ABG"
lastSentence = "The ""ABG"" Champion balances the big picture view with the task close at hand and readily" _
& " accepts the differences of others."
    Case "AABG"
lastSentence = "The ""AABG"" Champion balances the big picture view with the task close at hand and readily" _
& " accepts the differences of others."
    Case "AAABG"
lastSentence = "The ""AAABG"" Champion balances the big picture view with the task close at hand and readily" _
& " accepts the differences of others."
    Case "AB"
lastSentence = "The ""AB"" Champion sees the big picture and readily accepts the differences of others."
    Case "AAB"
lastSentence = "The ""AAB"" Champion sees the big picture and readily accepts the differences of others."
    Case "AAAB"
lastSentence = "The ""AAAB"" Champion sees the big picture and readily accepts the differences of others."
    Case "ABB"
lastSentence = "The ""ABB"" Champion sees the big picture clearly and readily accepts the differences of others."
    Case "AABB"
lastSentence = "The ""AABB"" Champion sees the big picture clearly and readily accepts the differences of others."
    Case "AAABB"
lastSentence = "The ""AAABB"" Champion sees the big picture clearly and readily accepts the differences of others."
    Case "ABBB"
lastSentence = "The ""ABBB"" Champion sees the big picture very clearly and readily accepts the differences of others."
    Case "AABBB"
lastSentence = "The ""AABBB"" Champion sees the big picture very clearly and readily accepts the differences of others."
    Case "AAABBB"
lastSentence = "The ""AAABBB"" Champion sees the big picture very clearly and readily accepts the differences of others."
    Case "AG"
lastSentence = "The ""AG"" Champion focuses on the task close at hand and readily accepts the differences" _
& " of others."
    Case "AAG"
lastSentence = "The ""AAG"" Champion focuses on the task close at hand and readily accepts the differences" _
& " of others."
    Case "AAAG"
lastSentence = "The ""AAAG"" Champion focuses on the task close at hand and readily accepts the differences" _
& " of others."
    Case "AGG"
lastSentence = "The ""AGG"" Champion focuses very closely on the task at hand and readily accepts the differences" _
& " of others."
    Case "AAGG"
lastSentence = "The ""AAGG"" Champion focuses very closely on the task at hand and readily accepts the differences" _
& " of others."
    Case "AAAGG"
lastSentence = "The ""AAAGG"" Champion focuses very closely on the task at hand and readily accepts the differences" _
& " of others."
    Case "AGGG"
lastSentence = "The ""AGGG"" Champion focuses extremely closely on the task at hand and readily accepts the differences" _
& " of others."
    Case "AAGGG"
lastSentence = "The ""AAGGG"" Champion focuses extremely closely on the task at hand and readily accepts the differences" _
& " of others."
    Case "AAAGGG"
lastSentence = "The ""AAAGGG"" Champion focuses extremely closely on the task at hand and readily accepts the differences" _
& " of others."
    Case Else
lastSentence = "This Type needs to be added"
End Select
End If

description = description & "   " & lastSentence
ENFPParagraph = description
End Function
Function ENFJParagraph(Qfirst, QandA)

'needs to be double q to get first one...
If (InStr(2, QandA, "Q") = 2) Then
description = "Technical Teachers are born educators with the natural ability to influence others and" _
& " to lead them towards learning scientific, mathematical, and complex concepts, often without seeming" _
& " to do so.   These smooth-talking persuaders are life's sales engineers.   Idealists at heart, their" _
& " enthusiastic and charismatic manner is inspiring.   People are their highest priority.   Technical" _
& " Teachers naturally communicate personal concern and a willingness to become involved." _

Select Case QandA
    Case "QQBG"
lastSentence = "The ""QQBG"" Technical Teacher balances the big picture view with the task close at hand and is particularly adept at solving problems with little pre-planning."
    Case "QQQBG"
lastSentence = "The ""QQQBG"" Technical Teacher balances the big picture view with the task close at hand and is extremely adept at solving problems with little pre-planning."
     Case "QQB"
lastSentence = "The ""QQB"" Technical Teacher sees the big picture and is particularly adept at solving problems with little pre-planning."
    Case "QQQB"
lastSentence = "The ""QQQB"" Technical Teacher sees the big picture and is extremely adept at solving problems with little pre-planning."
    Case "QQBB"
lastSentence = "The ""QQBB"" Technical Teacher sees the big picture clearly and is particularly adept at solving problems with little pre-planning."
    Case "QQQBB"
lastSentence = "The ""QQQBB"" Technical Teacher sees the big picture clearly and is extremely adept at solving problems with little pre-planning."
    Case "QQBBB"
lastSentence = "The ""QQBBB"" Technical Teacher sees the big picture very clearly and is particularly adept at solving problems with little pre-planning."
    Case "QQQBBB"
lastSentence = "The ""QQQBBB"" Technical Teacher sees the big picture very clearly and is extremely adept at solving problems with little pre-planning."
    Case "QQG"
lastSentence = "The ""QQG"" Technical Teacher focuses on the task close at hand and is particularly adept at solving problems with little pre-planning."
    Case "QQQG"
lastSentence = "The ""QQQG"" Technical Teacher focuses on the task close at hand and is extremely adept at solving problems with little pre-planning."
    Case "QQGG"
lastSentence = "The ""QQGG"" Technical Teacher focuses very closely on the task at hand and is particularly adept at solving problems with little pre-planning."
    Case "QQQGG"
lastSentence = "The ""QQQGG"" Technical Teacher focuses very closely on the task at hand and is extremely adept at solving problems with little pre-planning."
    Case "QQGGG"
lastSentence = "The ""QQGGG"" Technical Teacher focuses extremely closely on the task at hand and is particularly adept at solving problems with little pre-planning."
    Case "QQQGGG"
lastSentence = "The ""QQQGGG"" Technical Teacher focuses extremely closely on the task at hand and is extremely adept at solving problems with little pre-planning."
    Case Else
lastSentence = "This Type needs to be added"
End Select

Else

description = "Teachers are born educators with the natural ability to influence others and to lead them" _
& " towards learning, often without seeming to do so.   These smooth-talking persuaders are life's" _
& " salespeople.  Idealists at heart, their enthusiastic and charismatic manner is inspiring.   People" _
& " are their highest priority.   Teachers naturally communicate personal concern and a willingness to" _
& " become involved."

Select Case QandA
    Case "QBG"
lastSentence = "The ""QBG"" Teacher balances the big picture view with the task close at hand" _
& " and is adept at solving problems with little pre-planning."
    Case "ABG"
lastSentence = "The ""ABG"" Teacher balances the big picture view with the task close at hand" _
& " and readily accept new concepts and ideas."
    Case "AABG"
lastSentence = "The ""AABG"" Teacher balances the big picture view with the task close at hand" _
& " and readily accept new concepts and ideas."
    Case "AAABG"
lastSentence = "The ""AAABG"" Teacher balances the big picture view with the task close at hand" _
& " and readily accept new concepts and ideas."
    Case "QB"
lastSentence = "The ""QB"" Teacher sees the big picture and is adept at solving problems with little pre-planning."
    Case "AB"
lastSentence = "The ""AB"" Teacher sees the big picture and readily accepts new concepts and ideas."
    Case "AAB"
lastSentence = "The ""AAB"" Teacher sees the big picture and readily accepts new concepts and ideas."
    Case "AAAB"
lastSentence = "The ""AAAB"" Teacher sees the big picture and readily accepts new concepts and ideas."
    Case "ABB"
lastSentence = "The ""ABB"" Teacher sees the big picture clearly and readily accepts new concepts and ideas."
    Case "AABB"
lastSentence = "The ""AABB"" Teacher sees the big picture clearly and readily accepts new concepts and ideas."
    Case "AAABB"
lastSentence = "The ""AAABB"" Teacher sees the big picture clearly and readily accepts new concepts and ideas."
    Case "ABBB"
lastSentence = "The ""ABBB"" Teacher sees the big picture very clearly and readily accepts new concepts and ideas."
    Case "AABBB"
lastSentence = "The ""AABBB"" Teacher sees the big picture very clearly and readily accepts new concepts and ideas."
    Case "AAABBB"
lastSentence = "The ""AAABBB"" Teacher sees the big picture very clearly and readily accepts new concepts and ideas."
    Case "QG"
lastSentence = "The ""QG"" Teacher focuses on the task close at hand and is adept at solving problems with little pre-planning."
    Case "AG"
lastSentence = "The ""AG"" Teacher focuses on the task close at hand and readily accepts new concepts" _
& " and ideas."
    Case "AAG"
lastSentence = "The ""AAG"" Teacher focuses on the task close at hand and readily accepts new concepts" _
& " and ideas."
    Case "AAAG"
lastSentence = "The ""AAAG"" Teacher focuses on the task close at hand and readily accepts new concepts" _
& " and ideas."
    Case "AGG"
lastSentence = "The ""AGG"" Teacher focuses very closely on the task at hand and readily accepts new concepts" _
& " and ideas."
    Case "AAGG"
lastSentence = "The ""AAGG"" Teacher focuses very closely on the task at hand and readily accepts new concepts" _
& " and ideas."
    Case "AAAGG"
lastSentence = "The ""AAAGG"" Teacher focuses very closely on the task at hand and readily accepts new concepts" _
& " and ideas."
    Case "AGGG"
lastSentence = "The ""AGGG"" Teacher focuses extremely closely on the task at hand and readily accepts new concepts" _
& " and ideas."
    Case "AAGGG"
lastSentence = "The ""AAGGG"" Teacher focuses extremely closely on the task at hand and readily accepts new concepts" _
& " and ideas."
    Case "AAAGGG"
lastSentence = "The ""AAAGGG"" Teacher focuses extremely closely on the task at hand and readily accepts new concepts" _
& " and ideas."
    Case Else
lastSentence = "This Type needs to be added"
End Select

End If

description = description & "   " & lastSentence
ENFJParagraph = description
End Function

Function INTJParagraph(Qfirst, QandA)

If Qfirst Then

description = "Masterminds are excellent at planning operations.  They're way above the other types in" _
& " ""if-then"" contingency planning.   They always have a Plan A, but are prepared to switch to Plan" _
& " B, C, or D.  Being independent and highly self-confident, they are very open-minded and will entertain" _
& " any idea that might prove useful.   Masterminds tend to verbalize the positive and avoid negative comments." _

Select Case QandA
    Case "QBG"
lastSentence = "The ""QBG"" Mastermind balances the big picture view with the task close at hand and is" _
& " adept at planning analysis."
    Case "QQBG"
lastSentence = "The ""QQBG"" Mastermind balances the big picture view with the task close at hand and is" _
& " particularly adept at planning analysis."
    Case "QQQBG"
lastSentence = "The ""QQQBG"" Mastermind balances the big picture view with the task close at hand and is" _
& " extrememly adept at planning analysis."
    Case "QB"
lastSentence = "The ""QB"" Mastermind sees the big picture and is adept at planning analysis."
    Case "QQB"
lastSentence = "The ""QQB"" Mastermind sees the big picture and is particularly adept at planning analysis."
    Case "QQQB"
lastSentence = "The ""QQQB"" Mastermind sees the big picture and is extremely adept at planning analysis."
    Case "QBB"
lastSentence = "The ""QBB"" Mastermind sees the big picture clearly and is adept at planning analysis."
    Case "QQBB"
lastSentence = "The ""QQBB"" Mastermind sees the big picture clearly and is particularly adept at planning analysis."
    Case "QQQBB"
lastSentence = "The ""QQQBB"" Mastermind sees the big picture clearly and is extremely adept at planning analysis."
    Case "QBBB"
lastSentence = "The ""QBBB"" Mastermind sees the big picture very clearly and is adept at planning analysis."
    Case "QQBBB"
lastSentence = "The ""QQBBB"" Mastermind sees the big picture very clearly and is particularly adept at planning analysis."
    Case "QQQBBB"
lastSentence = "The ""QQQBBB"" Mastermind sees the big picture very clearly and is extremely adept at planning analysis."
    Case "QG"
lastSentence = "The ""QG"" Mastermind focuses on the task close at hand and is adept at planning" _
& " analysis."
    Case "QQG"
lastSentence = "The ""QQG"" Mastermind focuses on the task close at hand and is particularly adept at planning" _
& " analysis."
    Case "QQQG"
lastSentence = "The ""QQQG"" Mastermind focuses on the task close at hand and is extremely adept at planning" _
& " analysis."
    Case "QGG"
lastSentence = "The ""QGG"" Mastermind focuses very closely on the task at hand and is adept at planning" _
& " analysis."
    Case "QQGG"
lastSentence = "The ""QQGG"" Mastermind focuses very closely on the task at hand and is particularly adept at planning" _
& " analysis."
    Case "QQQGG"
lastSentence = "The ""QQQGG"" Mastermind focuses very closely on the task at hand and is extremely adept at planning" _
& " analysis."
    Case "QGGG"
lastSentence = "The ""QGGG"" Mastermind focuses extremely closely on the task at hand and is adept at planning" _
& " analysis."
    Case "QQGGG"
lastSentence = "The ""QQGGG"" Mastermind focuses extremely closely on the task at hand and is particularly adept at planning" _
& " analysis."
    Case "QQQGGG"
lastSentence = "The ""QQQGGG"" Mastermind focuses extremely closely on the task at hand and is exceptionally adept at planning" _
& " analysis."
    Case Else
lastSentence = "This Type needs to be added"
End Select

Else

description = "Diplomats are flexible, reserved, and accepting.   They have an unusually strong desire to contribute to the" _
& " welfare of their country and the world.   They quietly exert their influence behind the scenes.   Their quiet strength is felt" _
& " by others.   They are preoccupied with understanding and dealing with complex ethical issues.   They are open-minded and will" _
& " entertain any idea that may prove useful."

Select Case QandA
    Case "ABG"
lastSentence = "The ""ABG"" Diplomat balances the big picture view with the task close at hand and" _
& " readily accepts different views."
    Case "AABG"
lastSentence = "The ""AABG"" Diplomat balances the big picture view with the task close at hand and" _
& " readily accepts different views."
    Case "AAABG"
lastSentence = "The ""AAABG"" Diplomat balances the big picture view with the task close at hand and" _
& " readily accepts different views."
    Case "AB"
lastSentence = "The ""AB"" Diplomat sees the big picture and readily accepts different views."
    Case "AAB"
lastSentence = "The ""AAB"" Diplomat sees the big picture and readily accepts different views."
    Case "AAAB"
lastSentence = "The ""AAAB"" Diplomat sees the big picture and readily accepts different views."
    Case "ABB"
lastSentence = "The ""ABB"" Diplomat sees the big picture clearly and readily accepts different views."
    Case "AABB"
lastSentence = "The ""AABB"" Diplomat sees the big picture clearly and readily accepts different views."
    Case "AAABB"
lastSentence = "The ""AAABB"" Diplomat sees the big picture clearly and readily accepts different views."
    Case "ABBB"
lastSentence = "The ""ABBB"" Diplomat sees the big picture very clearly and readily accepts different views."
    Case "AABBB"
lastSentence = "The ""AABBB"" Diplomat sees the big picture very clearly and readily accepts different views."
    Case "AAABBB"
lastSentence = "The ""AAABBB"" Diplomat sees the big picture very clearly and readily accepts different views."
    Case "AG"
lastSentence = "The ""AG"" Diplomat focuses on the task close at hand and" _
& " readily accepts different views."
    Case "AAG"
lastSentence = "The ""AAG"" Diplomat focuses on the task close at hand and" _
& " readily accepts different views."
    Case "AAAG"
lastSentence = "The ""AAAG"" Diplomat focuses on the task close at hand and" _
& " readily accepts different views."
    Case "AGG"
lastSentence = "The ""AGG"" Diplomat focuses very closely on the task at hand and" _
& " readily accepts different views."
    Case "AAGG"
lastSentence = "The ""AAGG"" Diplomat focuses very closely on the task at hand and" _
& " readily accepts different views."
    Case "AAAGG"
lastSentence = "The ""AAAGG"" Diplomat focuses very closely on the task at hand and" _
& " readily accepts different views."
    Case "AGGG"
lastSentence = "The ""AGGG"" Diplomat focuses extremely closely on the task at hand and" _
& " readily accepts different views."
    Case "AAGGG"
lastSentence = "The ""AAGGG"" Diplomat focuses extremely closely on the task at hand and" _
& " readily accepts different views."
    Case "AAAGGG"
lastSentence = "The ""AAAGGG"" Diplomat focuses extremely closely on the task at hand and" _
& " readily accepts different views."
End Select

End If

description = description & "   " & lastSentence
INTJParagraph = description
End Function

Function INTPParagraph(Qfirst, QandA)

If Qfirst Then

description = "Scientists are the architects of all kinds of theoretical systems.   The world exists" _
& " to be analyzed, understood and explained.   Curiosity about the world's fundamental principles and" _
& " natural laws is their driving force.   They are ruthless pragmatists about ideas.   Of all the types," _
& " they exhibit the greatest precision in thought and speech.   Scientists see inconsistencies instantly and are" _
& " devastating in debate." _

Select Case QandA
    Case "QBG"
lastSentence = "The ""QBG"" Scientist balances the big picture view with the task close at hand and is adept at systems analysis."
    Case "QQBG"
lastSentence = "The ""QQBG"" Scientist balances the big picture view with the task close at hand and is particularly adept at systems analysis."
    Case "QQQBG"
lastSentence = "The ""QQQBG"" Scientist balances the big picture view with the task close at hand and is extremely adept at systems analysis."
    Case "QB"
lastSentence = "The ""QB"" Scientist sees the big picture and is adept at systems analysis."
    Case "QQB"
lastSentence = "The ""QQB"" Scientist sees the big picture and is particularly adept at systems analysis."
    Case "QQQB"
lastSentence = "The ""QQQB"" Scientist sees the big picture and is extremely adept at systems analysis."
    Case "QBB"
lastSentence = "The ""QBB"" Scientist sees the big picture clearly and is adept at systems analysis."
    Case "QQBB"
lastSentence = "The ""QQBB"" Scientist sees the big picture clearly and is particularly adept at systems analysis."
    Case "QQQBB"
lastSentence = "The ""QQQBB"" Scientist sees the big picture clearly and is extremely adept at systems analysis."
    Case "QBBB"
lastSentence = "The ""QBBB"" Scientist sees the big picture very clearly and is adept at systems analysis."
    Case "QQBBB"
lastSentence = "The ""QQBBB"" Scientist sees the big picture very clearly and is particularly adept at systems analysis."
    Case "QQQBBB"
lastSentence = "The ""QQQBBB"" Scientist sees the big picture very clearly and is extremely adept at systems analysis."
    Case "QG"
lastSentence = "The ""QG"" Scientist focuses on the task close at hand and is adept at systems analysis."
    Case "QQG"
lastSentence = "The ""QQG"" Scientist focuses on the task close at hand and is particularly adept at systems analysis."
    Case "QQQG"
lastSentence = "The ""QQQG"" Scientist focuses on the task close at hand and is extremely adept at systems analysis."
    Case "QGG"
lastSentence = "The ""QGG"" Scientist focuses very closely on the task at hand and is adept at systems analysis."
    Case "QQGG"
lastSentence = "The ""QQGG"" Scientist focuses very closely on the task at hand and is particularly adept at systems analysis."
    Case "QQQGG"
lastSentence = "The ""QQQGG"" Scientist focuses very closely on the task at hand and is extremely adept at systems analysis."
    Case "QGGG"
lastSentence = "The ""QGGG"" Scientist focuses extremely closely on the task at hand and is adept at systems analysis."
    Case "QQGGG"
lastSentence = "The ""QQGGG"" Scientist focuses extremely closely on the task at hand and is particularly adept at systems analysis."
    Case "QQQGGG"
lastSentence = "The ""QQQGGG"" Scientist focuses extremely closely on the task at hand and is exceptionally adept at systems analysis."
    
    Case Else
lastSentence = "This Type needs to be added"
End Select

Else

description = "Idealists are easygoing, flexible, tranquil, gracious and adaptive.   Yet, when their" _
& " self-imposed codes are trampled on, they can become very aggressive and demanding.   They see the" _
& " world as wide open and full of possibilities and potentials.  They welcome new ideas and information." _
& "   Idealists are willing to make extraordinary sacrifices for causes in which they believe strongly." _

Select Case QandA
    Case "ABG"
    lastSentence = "The ""ABG"" Idealist balances the big picture view with the task close at hand and readily endorses new ideas and" _
& " concepts."
    Case "AABG"
    lastSentence = "The ""AABG"" Idealist balances the big picture view with the task close at hand and readily endorses new ideas and" _
& " concepts."
    Case "AAABG"
    lastSentence = "The ""AAABG"" Idealist balances the big picture view with the task close at hand and readily endorses new ideas and" _
& " concepts."
    Case "AB"
    lastSentence = "The ""AB"" Idealist sees the big picture and readily endorses new ideas and" _
& " concepts."
    Case "AAB"
    lastSentence = "The ""AAB"" Idealist sees the big picture and readily endorses new ideas and" _
& " concepts."
    Case "AAAB"
    lastSentence = "The ""AAAB"" Idealist sees the big picture and readily endorses new ideas and" _
& " concepts."
    Case "ABB"
    lastSentence = "The ""ABB"" Idealist sees the big picture clearly and readily endorses new ideas and" _
& " concepts."
    Case "AABB"
    lastSentence = "The ""AABB"" Idealist sees the big picture clearly and readily endorses new ideas and" _
& " concepts."
    Case "AAABB"
    lastSentence = "The ""AAABB"" Idealist sees the big picture clearly and readily endorses new ideas and" _
& " concepts."
    Case "ABBB"
    lastSentence = "The ""ABBB"" Idealist sees the big picture very clearly and readily endorses new ideas and" _
& " concepts."
    Case "AABBB"
    lastSentence = "The ""AABBB"" Idealist sees the big picture very clearly and readily endorses new ideas and" _
& " concepts."
    Case "AAABBB"
    lastSentence = "The ""AAABBB"" Idealist sees the big picture very clearly and readily endorses new ideas and" _
& " concepts."
    Case "AG"
    lastSentence = "The ""AG"" Idealist focuses on the task close at hand and readily endorses new ideas and" _
& " concepts."
    Case "AAG"
    lastSentence = "The ""AAG"" Idealist focuses on the task close at hand and readily endorses new ideas and" _
& " concepts."
    Case "AAAG"
    lastSentence = "The ""AAAG"" Idealist focuses on the task close at hand and readily endorses new ideas and" _
& " concepts."
    Case "AGG"
    lastSentence = "The ""AGG"" Idealist focuses very closely on the task at hand and readily endorses new ideas and" _
& " concepts."
    Case "AAGG"
    lastSentence = "The ""AAGG"" Idealist focuses very closely on the task at hand and readily endorses new ideas and" _
& " concepts."
    Case "AAAGG"
    lastSentence = "The ""AAAGG"" Idealist focuses very closely on the task at hand and readily endorses new ideas and" _
& " concepts."
    Case "AGGG"
    lastSentence = "The ""AGGG"" Idealist focuses extremely closely on the task at hand and readily endorses new ideas and" _
& " concepts."
    Case "AAGGG"
    lastSentence = "The ""AAGGG"" Idealist focuses extremely closely on the task at hand and readily endorses new ideas and" _
& " concepts."
    Case "AAAGGG"
    lastSentence = "The ""AAAGGG"" Idealist focuses extremely closely on the task at hand and readily endorses new ideas and" _
& " concepts."

    Case Else
lastSentence = "This Type needs to be added"
End Select

End If

description = description & "   " & lastSentence
INTPParagraph = description
End Function

Function ENTPParagraph(Qfirst, QandA)

If Qfirst Then

description = "Strategists have natural ingenuity.  They are intensely curious and continually probe" _
& " for solutions to complex problems.   They are always on the lookout for a better way.   They are" _
& " confident in their inventiveness and display an extraordinary talent for rising to the demands of" _
& " even the most impossible situation.  Rather than make a detailed plan, Strategists count on their ability" _
& " to solve problems as they arise." _

Select Case QandA
    Case "QBG"
lastSentence = "The ""QBG"" Strategist balances the big picture view with the task close at hand and is adept at dissecting" _
& " problems."
    Case "QQBG"
lastSentence = "The ""QQBG"" Strategist balances the big picture view with the task close at hand and is particularly adept at dissecting" _
& " problems."
    Case "QQQBG"
lastSentence = "The ""QQQBG"" Strategist balances the big picture view with the task close at hand and is extremely adept at dissecting" _
& " problems."
    Case "QB"
lastSentence = "The ""QB"" Strategist sees the big picture and is adept at analyzing and dissecting problems."
    Case "QQB"
lastSentence = "The ""QQB"" Strategist sees the big picture and is particularly adept at analyzing and dissecting problems."
    Case "QQQB"
lastSentence = "The ""QQQB"" Strategist sees the big picture and is extremely adept at analyzing and dissecting problems."
    Case "QBB"
lastSentence = "The ""QBB"" Strategist sees the big picture clearly and is adept at analyzing and dissecting problems."
    Case "QQBB"
lastSentence = "The ""QQBB"" Strategist sees the big picture clearly and is particularly adept at analyzing and dissecting problems."
    Case "QQQBB"
lastSentence = "The ""QQQBB"" Strategist sees the big picture clearly and is extremely adept at analyzing and dissecting problems."
    Case "QBBB"
lastSentence = "The ""QBBB"" Strategist sees the big picture very clearly and is adept at analyzing and dissecting problems."
    Case "QQBBB"
lastSentence = "The ""QQBBB"" Strategist sees the big picture very clearly and is particularly adept at analyzing and dissecting problems."
    Case "QQQBBB"
lastSentence = "The ""QQQBBB"" Strategist sees the big picture very clearly and is extremely adept at analyzing and dissecting problems."
    Case "QG"
lastSentence = "The ""QG"" Strategist focuses on the task close at hand and is adept at dissecting" _
& " problems."
    Case "QG"
lastSentence = "The ""QG"" Strategist focuses on the task close at hand and is adept at dissecting" _
& " problems."
    Case "QQG"
lastSentence = "The ""QQG"" Strategist focuses on the task close at hand and is particularly adept at dissecting" _
& " problems."
    Case "QQQG"
lastSentence = "The ""QQQG"" Strategist focuses on the task close at hand and is extremely adept at dissecting" _
& " problems."
    Case "QGG"
lastSentence = "The ""QGG"" Strategist focuses very closely on the task at hand and is adept at dissecting" _
& " problems."
    Case "QQGG"
lastSentence = "The ""QQGG"" Strategist focuses very closely on the task at hand and is particularly adept at dissecting" _
& " problems."
    Case "QQQGG"
lastSentence = "The ""QQQGG"" Strategist focuses very closely on the task at hand and is extremely adept at dissecting" _
& " problems."
    Case "QGGG"
lastSentence = "The ""QGGG"" Strategist focuses extremely closely on the task at hand and is adept at dissecting" _
& " problems."
    Case "QQGGG"
lastSentence = "The ""QQGGG"" Strategist focuses extremely closely on the task at hand and is particularly adept at dissecting" _
& " problems."
    Case "QQQGGG"
lastSentence = "The ""QQQGGG"" Strategist focuses extremely closely on the task at hand and is extremely adept at dissecting" _
& " problems."

    Case Else
lastSentence = "This Type needs to be added"
End Select
    
Else

description = "Idea Generators are dynamic, enthusiastic, and gregarious." _
& "  Their zest for life is boundless, making them one of the most outgoing of all the types.   They" _
& " love generating new ideas and upsetting boring routines.   They are very affirming and complimentary." _
& "  They are naturally irreverent.  Their language is peppered with humor and metaphors.   Idea Generators" _
& " are insatiably curious about things and are quick to strike up conversations with strangers, asking a lot" _
& " of questions."


Select Case QandA
    Case "ABG"
lastSentence = "The ""ABG"" Idea Generator balances the big picture view with the task close at hand and readily endorses new ideas and concepts."
    Case "AABG"
lastSentence = "The ""AABG"" Idea Generator balances the big picture view with the task close at hand and readily endorses new ideas and concepts."
    Case "AAABG"
lastSentence = "The ""AAABG"" Idea Generator balances the big picture view with the task close at hand and readily endorses new ideas and concepts."
    Case "AB"
lastSentence = "The ""AB"" Idea Generator sees the big picture and readily endorses new ideas and concepts."
    Case "AAB"
lastSentence = "The ""AAB"" Idea Generator sees the big picture and readily endorses new ideas and concepts."
    Case "AAAB"
lastSentence = "The ""AAAB"" Idea Generator sees the big picture and readily endorses new ideas and concepts."
    Case "ABB"
lastSentence = "The ""ABB"" Idea Generator sees the big picture clearly and readily endorses new ideas and concepts."
    Case "AABB"
lastSentence = "The ""AABB"" Idea Generator sees the big picture clearly and readily endorses new ideas and concepts."
    Case "AAABB"
lastSentence = "The ""AAABB"" Idea Generator sees the big picture clearly and readily endorses new ideas and concepts."
    Case "ABBB"
lastSentence = "The ""ABBB"" Idea Generator sees the big picture very clearly and readily endorses new ideas and concepts."
    Case "AABBB"
lastSentence = "The ""AABBB"" Idea Generator sees the big picture very clearly and readily endorses new ideas and concepts."
    Case "AAABBB"
lastSentence = "The ""AAABBB"" Idea Generator sees the big picture very clearly and readily endorses new ideas and concepts."
    Case "AG"
lastSentence = "The ""AG"" Idea Generator focuses on the task close at hand and readily endorses new ideas and concepts."
    Case "AAG"
lastSentence = "The ""AAG"" Idea Generator focuses on the task close at hand and readily endorses new ideas and concepts."
    Case "AAAG"
lastSentence = "The ""AAAG"" Idea Generator focuses on the task close at hand and readily endorses new ideas and concepts."
    Case "AGG"
lastSentence = "The ""AGG"" Idea Generator focuses very closely on the task at hand and readily endorses new ideas and concepts."
    Case "AAGG"
lastSentence = "The ""AAGG"" Idea Generator focuses very closely on the task at hand and readily endorses new ideas and concepts."
    Case "AAAGG"
lastSentence = "The ""AAAGG"" Idea Generator focuses very closely on the task at hand and readily endorses new ideas and concepts."
    Case "AGGG"
lastSentence = "The ""AGGG"" Idea Generator focuses extremely closely on the task at hand and readily endorses new ideas and concepts."
    Case "AAGGG"
lastSentence = "The ""AAGGG"" Idea Generator focuses extremely closely on the task at hand and readily endorses new ideas and concepts."
    Case "AAAGGG"
lastSentence = "The ""AAAGGG"" Idea Generator focuses extremely closely on the task at hand and readily endorses new ideas and concepts."
  
    Case Else
lastSentence = "This Type needs to be added"
End Select

End If

description = description & "   " & lastSentence
ENTPParagraph = description
End Function

Function ENTJParagraph(Qfirst, QandA)

If Qfirst Then

description = "Field Marshals are born leaders, ready, willing, and able to command people.   They have a" _
& " strong natural urge to bring order and efficiency wherever they are - to harness people and resources" _
& " and to logically lead them towards their goals with maximum speed and minimum wasted effort.  They" _
& " are energetic, robust, and argumentative.   They are superb executives and supreme pragmatists." _

Select Case QandA
    Case "QBG"
lastSentence = "The ""QBG"" Field Marshal balances the big picture view with the task close at hand and" _
& " is analytical, questioning everything."
    Case "QQBG"
lastSentence = "The ""QQBG"" Field Marshal balances the big picture view with the task close at hand and" _
& " is very analytical, questioning everything."
    Case "QQQBG"
lastSentence = "The ""QQQBG"" Field Marshal balances the big picture view with the task close at hand and" _
& " is extremely analytical, questioning everything."
    Case "QB"
lastSentence = "The ""QB"" Field Marshal sees the big picture and is analytical, questioning everything."
    Case "QQB"
lastSentence = "The ""QQB"" Field Marshal sees the big picture and is very analytical, questioning everything."
   Case "QQQB"
lastSentence = "The ""QQQB"" Field Marshal sees the big picture and is extremely analytical, questioning everything."
    Case "QBB"
lastSentence = "The ""QBB"" Field Marshal sees the big picture clearly and is analytical, questioning everything."
    Case "QQBB"
lastSentence = "The ""QQBB"" Field Marshal sees the big picture clearly and is very analytical, questioning everything."
   Case "QQQBB"
lastSentence = "The ""QQQBB"" Field Marshal sees the big picture clearly and is extremely analytical, questioning everything."
    Case "QBBB"
lastSentence = "The ""QBBB"" Field Marshal sees the big picture very clearly and is analytical, questioning everything."
    Case "QQBBB"
lastSentence = "The ""QQBBB"" Field Marshal sees the big picture very clearly and is very analytical, questioning everything."
   Case "QQQBBB"
lastSentence = "The ""QQQBBB"" Field Marshal sees the big picture very clearly and is extremely analytical, questioning everything."
    Case "QG"
lastSentence = "The ""QG"" Field Marshal focuses on the task close at hand and is analytical," _
& " questioning everything."
    Case "QQG"
lastSentence = "The ""QQG"" Field Marshal focuses on the task close at hand and is very analytical," _
& " questioning everything."
    Case "QQQG"
lastSentence = "The ""QQQG"" Field Marshal focuses on the task close at hand and is extremely analytical," _
& " questioning everything."
    Case "QGG"
lastSentence = "The ""QGG"" Field Marshal focuses very closely on the task at hand and is analytical," _
& " questioning everything."
    Case "QQGG"
lastSentence = "The ""QQGG"" Field Marshal focuses very closely on the task at hand and is very analytical," _
& " questioning everything."
    Case "QQQGG"
lastSentence = "The ""QQQGG"" Field Marshal focuses very closely on the task at hand and is extremely analytical," _
& " questioning everything."
    Case "QGGG"
lastSentence = "The ""QGGG"" Field Marshal focuses extremely closely on the task at hand and is analytical," _
& " questioning everything."
    Case "QQGGG"
lastSentence = "The ""QQGGG"" Field Marshal focuses extremely closely on the task at hand and is very analytical," _
& " questioning everything."
    Case "QQQGGG"
lastSentence = "The ""QQQGGG"" Field Marshal focuses extremely closely on the task at hand and is extremely analytical," _
& " questioning everything."
    Case Else
lastSentence = "This Type needs to be added"
End Select

Else

description = "Loyal Lieutenants are born followers - ready, willing, and able to unquestionably carry out their" _
& " orders.   They avoid confrontation whenever possible.   Friendly and outgoing, they try to cooperate and find" _
& " middle ground as long as it doesn't interfere with the dutiful execution of their orders.    They are very loyal" _
& " to their supervisors or leaders and willingly accept their direction and decisions."

Select Case QandA
    Case "ABG"
lastSentence = "The ""ABG"" Loyal Lieutenant balances the big picture view with the task close at hand and" _
& " readily accepts new commands."
    Case "AABG"
lastSentence = "The ""AABG"" Loyal Lieutenant balances the big picture view with the task close at hand and" _
& " readily accepts new commands."
    Case "AAABG"
lastSentence = "The ""AAABG"" Loyal Lieutenant balances the big picture view with the task close at hand and" _
& " readily accepts new commands."
    Case "AB"
lastSentence = "The ""AB"" Loyal Lieutenant sees the big picture and readily accepts new commands."
    Case "AAB"
lastSentence = "The ""AAB"" Loyal Lieutenant sees the big picture and readily accepts new commands."
    Case "AAAB"
lastSentence = "The ""AAAB"" Loyal Lieutenant sees the big picture and readily accepts new commands."
    Case "ABB"
lastSentence = "The ""ABB"" Loyal Lieutenant sees the big picture clearly and readily accepts new commands."
    Case "AABB"
lastSentence = "The ""AABB"" Loyal Lieutenant sees the big picture clearly and readily accepts new commands."
    Case "AAABB"
lastSentence = "The ""AAABB"" Loyal Lieutenant sees the big picture clearly and readily accepts new commands."
    Case "ABBB"
lastSentence = "The ""ABBB"" Loyal Lieutenant sees the big picture very clearly and readily accepts new commands."
    Case "AABBB"
lastSentence = "The ""AABBB"" Loyal Lieutenant sees the big picture very clearly and readily accepts new commands."
    Case "AAABBB"
lastSentence = "The ""AAABBB"" Loyal Lieutenant sees the big picture very clearly and readily accepts new commands."
    Case "AG"
lastSentence = "The ""AG"" Loyal Lieutenant focuses on the task close at hand and readily accepts new commands."
    Case "AAG"
lastSentence = "The ""AAG"" Loyal Lieutenant focuses on the task close at hand and readily accepts new commands."
    Case "AAAG"
lastSentence = "The ""AAAG"" Loyal Lieutenant focuses on the task close at hand and readily accepts new commands."
    Case "AGG"
lastSentence = "The ""AGG"" Loyal Lieutenant focuses very closely on the task at hand and readily accepts new commands."
    Case "AAGG"
lastSentence = "The ""AAGG"" Loyal Lieutenant focuses very closely on the task at hand and readily accepts new commands."
    Case "AAAGG"
lastSentence = "The ""AAAGG"" Loyal Lieutenant focuses very closely on the task at hand and readily accepts new commands."
    Case "AGGG"
lastSentence = "The ""AGGG"" Loyal Lieutenant focuses extremely closely on the task at hand and readily accepts new commands."
    Case "AAGGG"
lastSentence = "The ""AAGGG"" Loyal Lieutenant focuses extremely closely on the task at hand and readily accepts new commands."
    Case "AAAGGG"
lastSentence = "The ""AAAGGG"" Loyal Lieutenant focuses extremely closely on the task at hand and readily accepts new commands."
End Select


End If

description = description & "   " & lastSentence
ENTJParagraph = description
End Function

Function ISTPParagraph(Qfirst, QandA)

If Qfirst Then

description = "Crafters are reserved, aloof and interpersonally cautious, yet ready to try anything once." _
& " They thrive on excitement and risk-taking.   They are masterful operators of tools, equipment, machines," _
& " weapons, and instruments of all kinds.  Crafters are fearless in action and very impulsive.   They can" _
& " be fiercely insubordinate.  They want to follow their own lead and do not want to be subject to any rules" _
& " or laws." _

Select Case QandA
    Case "QBG"
lastSentence = "The ""QBG"" Crafter balances the big picture view with the task close at hand and is adept at diagnosing problems with tools and machinery."
    Case "QQBG"
lastSentence = "The ""QQBG"" Crafter balances the big picture view with the task close at hand and is particularly adept at diagnosing problems with tools and machinery."
    Case "QQQBG"
lastSentence = "The ""QQQBG"" Crafter balances the big picture view with the task close at hand and is extremely adept at diagnosing problems with tools and machinery."
    Case "QB"
lastSentence = "The ""QB"" Crafter sees the big picture and is adept at diagnosing problems with tools and machinery."
    Case "QQB"
lastSentence = "The ""QQB"" Crafter sees the big picture and is particularly adept at diagnosing problems with tools and machinery."
    Case "QQQB"
lastSentence = "The ""QQQB"" Crafter sees the big picture and is extremely adept at diagnosing problems with tools and machinery."
    Case "QBB"
lastSentence = "The ""QBB"" Crafter sees the big picture clearly and is adept at diagnosing problems with tools and machinery."
    Case "QQBB"
lastSentence = "The ""QQBB"" Crafter sees the big picture clearly and is particularly adept at diagnosing problems with tools and machinery."
    Case "QQQBB"
lastSentence = "The ""QQQBB"" Crafter sees the big picture clearly and is extremely adept at diagnosing problems with tools and machinery."
    Case "QBBB"
lastSentence = "The ""QBBB"" Crafter sees the big picture very clearly and is adept at diagnosing problems with tools and machinery."
    Case "QQBBB"
lastSentence = "The ""QQBBB"" Crafter sees the big picture very clearly and is particularly adept at diagnosing problems with tools and machinery."
    Case "QQQBBB"
lastSentence = "The ""QQQBBB"" Crafter sees the big picture very clearly and is extremely adept at diagnosing problems with tools and machinery."
    Case "QG"
lastSentence = "The ""QG"" Crafter focuses on the task close at hand and is adept at diagnosing problems with tools and machinery."
    Case "QQG"
lastSentence = "The ""QQG"" Crafter focuses on the task close at hand and is particularly adept at diagnosing problems with tools and machinery."
    Case "QQQG"
lastSentence = "The ""QQQG"" Crafter focuses on the task close at hand and is extremely adept at diagnosing problems with tools and machinery."
    Case "QGG"
lastSentence = "The ""QGG"" Crafter focuses very closely on the task at hand and is adept at diagnosing problems with tools and machinery."
    Case "QQGG"
lastSentence = "The ""QQGG"" Crafter focuses very closely on the task at hand and is particularly adept at diagnosing problems with tools and machinery."
    Case "QQQGG"
lastSentence = "The ""QQQGG"" Crafter focuses very closely on the task at hand and is extremely adept at diagnosing problems with tools and machinery."
    Case "QGGG"
lastSentence = "The ""QGGG"" Crafter focuses extremely closely on the task at hand and is adept at diagnosing problems with tools and machinery."
    Case "QQGGG"
lastSentence = "The ""QQGGG"" Crafter focuses extremely closely on the task at hand and is particularly adept at diagnosing problems with tools and machinery."
    Case "QQQGGG"
lastSentence = "The ""QQQGGG"" Crafter focuses extremely closely on the task at hand and is extremely adept at diagnosing problems with tools and machinery."
    Case Else
lastSentence = "This Type needs to be added"
End Select

Else

description = "No description for A"
lastSentence = "No last sentence for A"

End If

description = description & "   " & lastSentence
ISTPParagraph = description
End Function

Function ESTPParagraph(Qfirst, QandA)

If Qfirst Then

description = "Promoters are gregarious, out-going, energetic, and active.   Of all the types, they are easily" _
& " the most persuasive.  They live in the moment and close to the edge.   They can be ruthless in action and" _
& " are usually willing to do whatever it takes to achieve their goals.   They are excellent trouble-shooters" _
& " and very tough negotiators.   They are sharp entrepreneurs, able to swing deals and kick-start enterprises." _

Select Case QandA
    Case "QBG"
lastSentence = "The ""QBG"" Promoter balances the big picture view with the task close at hand and is adept at analyzing and manipulating people."
    Case "QQBG"
lastSentence = "The ""QQBG"" Promoter balances the big picture view with the task close at hand and is particularly adept at analyzing and manipulating people."
    Case "QQQBG"
lastSentence = "The ""QQQBG"" Promoter balances the big picture view with the task close at hand and is extremely adept at analyzing and manipulating people."
    Case "QB"
lastSentence = "The ""QB"" Promoter sees the big picture and is adept at analyzing and manipulating" _
& " people."
    Case "QQB"
lastSentence = "The ""QQB"" Promoter sees the big picture and is particularly adept at analyzing and manipulating" _
& " people."
    Case "QQQB"
lastSentence = "The ""QQQB"" Promoter sees the big picture and is extremely adept at analyzing and manipulating" _
& " people."
    Case "QBB"
lastSentence = "The ""QBB"" Promoter sees the big picture clearly and is adept at analyzing and manipulating" _
& " people."
    Case "QQBB"
lastSentence = "The ""QQBB"" Promoter sees the big picture clearly and is particularly adept at analyzing and manipulating" _
& " people."
    Case "QQQBB"
lastSentence = "The ""QQQBB"" Promoter sees the big picture clearly and is extremely adept at analyzing and manipulating" _
& " people."
    Case "QG"
lastSentence = "The ""QG"" Promoter focuses the task close at hand and is adept at analyzing and manipulating people."
    Case "QQG"
lastSentence = "The ""QQG"" Promoter focuses the task close at hand and is particularly adept at analyzing and manipulating people."
    Case "QQQG"
lastSentence = "The ""QQQG"" Promoter focuses the task close at hand and is extremely adept at analyzing and manipulating people."
    Case "QGG"
lastSentence = "The ""QGG"" Promoter focuses very closely the task at hand and is adept at analyzing and manipulating people."
    Case "QQGG"
lastSentence = "The ""QQGG"" Promoter focuses very closely the task at hand and is particularly adept at analyzing and manipulating people."
    Case "QQQGG"
lastSentence = "The ""QQQGG"" Promoter focuses very closely the task at hand and is extremely adept at analyzing and manipulating people."
    Case "QGGG"
lastSentence = "The ""QGGG"" Promoter focuses extremely closely the task at hand and is adept at analyzing and manipulating people."
    Case "QQGGG"
lastSentence = "The ""QQGGG"" Promoter focuses extremely closely the task at hand and is particularly adept at analyzing and manipulating people."
    Case "QQQGGG"
lastSentence = "The ""QQQGGG"" Promoter focuses extremely closely the task at hand and is exceptionally adept at analyzing and manipulating people."
    Case Else
lastSentence = "This Type needs to be added"
End Select
Else

description = "Earthy Entertainers are free-spirited, fun-loving, impulsive and socially gregarious.  The world is their stage.  They love the" _
& " excitement of challenging an audience and quickly become the center of attention.   They are incurably optimistic.  Earthy Entertainers are" _
& " focused on the here-and-now.   They have a low tolerance for procedures, routines or anything else that stands in the way of immediate gratification."

Select Case QandA
    Case "ABG"
lastSentence = "The ""ABG"" Earthy Entertainer balances the big picture view with the task close at hand and is adept at lifting the spirits of others."
    Case "AABG"
lastSentence = "The ""AABG"" Earthy Entertainer balances the big picture view with the task close at hand and is particularly adept at lifting the spirits of others."
    Case "AAABG"
lastSentence = "The ""AAABG"" Earthy Entertainer balances the big picture view with the task close at hand and is extremely adept at lifting the spirits of others."
    Case "AB"
lastSentence = "The ""AB"" Earthy Entertainer sees the big picture and is adept at lifting the spirits of others."
    Case "AAB"
lastSentence = "The ""AAB"" Earthy Entertainer sees the big picture and is particularly adept at lifting the spirits of others."
    Case "AAAB"
lastSentence = "The ""AAAB"" Earthy Entertainer sees the big picture and is extremely adept at lifting the spirits of others."
    Case "ABB"
lastSentence = "The ""ABB"" Earthy Entertainer sees the big picture clearly and is adept at lifting the spirits of others."
    Case "AABB"
lastSentence = "The ""AABB"" Earthy Entertainer sees the big picture clearly and is particularly adept at lifting the spirits of others."
    Case "AAABB"
lastSentence = "The ""AAABB"" Earthy Entertainer sees the big picture clearly and is extremely adept at lifting the spirits of others."
    Case "ABBB"
lastSentence = "The ""ABBB"" Earthy Entertainer sees the big picture very clearly and is adept at lifting the spirits of others."
    Case "AABBB"
lastSentence = "The ""AABBB"" Earthy Entertainer sees the big picture very clearly and is particularly adept at lifting the spirits of others."
    Case "AAABBB"
lastSentence = "The ""AAABBB"" Earthy Entertainer sees the big picture very clearly and is extremely adept at lifting the spirits of others."
    Case "AG"
lastSentence = "The ""AG"" Earthy Entertainer focuses on the task close at hand and is adept at lifting the spirits of others."
    Case "AAG"
lastSentence = "The ""AAG"" Earthy Entertainer focuses on the task close at hand and is particularly adept at lifting the spirits of others."
    Case "AAAG"
lastSentence = "The ""AAAG"" Earthy Entertainer focuses on the task close at hand and is extremely adept at lifting the spirits of others."
    Case "AGG"
lastSentence = "The ""AGG"" Earthy Entertainer focuses very closely on the task at hand and is adept at lifting the spirits of others."
    Case "AAGG"
lastSentence = "The ""AAGG"" Earthy Entertainer focuses very closely on the task at hand and is particularly adept at lifting the spirits of others."
    Case "AAAGG"
lastSentence = "The ""AAAGG"" Earthy Entertainer focuses very closely on the task at hand and is extremely adept at lifting the spirits of others."
    Case "AGGG"
lastSentence = "The ""AGGG"" Earthy Entertainer focuses extremely closely on the task at hand and is adept at lifting the spirits of others."
    Case "AAGGG"
lastSentence = "The ""AAGGG"" Earthy Entertainer focuses extremely closely on the task at hand and is particularly adept at lifting the spirits of others."
    Case "AAAGGG"
lastSentence = "The ""AAAGGG"" Earthy Entertainer focuses extremely closely on the task at hand and is exceptionally adept at lifting the spirits of others."
End Select

End If

description = description & "   " & lastSentence
ESTPParagraph = description
End Function
