#!/usr/bin/python
import wx, cardlib

def pause(): x = raw_input('press enter to continue') 

def dlg(frame, msg, title=''):
    dlg = wx.MessageDialog(frame, msg, title, wx.OK | wx.ICON_INFORMATION)
    dlg.ShowModal()
    dlg.Destroy()

class TestFrame(wx.Frame):
    def __init__(self, parent):
        self.title = 'testing cardlib'
        self.cards = cardlib
        print self.title
        wx.Frame.__init__(self, parent, -1, self.title, size=(760, 580),
            style=wx.DEFAULT_FRAME_STYLE | wx.NO_FULL_REPAINT_ON_RESIZE)
        self.panel = wx.Panel(self, -1, size=(760,580))
        self.panel.SetBackgroundColour(wx.Colour(40,140,0))
        self.Centre()

    def doTest(self):
        dlg(self, 'We can create horizontal, vertical, \nand diagonal stacks of one or more cards')
        c = self.cards.stack(self.panel, [30,30], '12S', faceup=1)
        s0 = self.cards.stack(self.panel, [30, 160], ['12S', '10D'], [16,0])
        s1 = self.cards.stack(self.panel, (200, 30), ['0H', '4C', '5D'], [0,30])
        s2 = self.cards.stack(self.panel, (30, 300), ('3S', '4S', '5S'), \
            [30,30], faceup=0)
        dlg(self, 'We can flip cards')
        s1.flip()
        dlg(self, 'We can delete a stack')
        s0.clear()
        dlg(self, 'We can resize cards')
        s3 = self.cards.stack(self.panel, (300, 300), ['9H', '12S'], [10,0],\
            size=(10,20))

class TestApp(wx.App):
    def OnInit(self):
        #wx.Image_AddHandler(wx.PNGHandler())
        wx.InitAllImageHandlers()
        frame = TestFrame(None)
        frame.Show(True)
        self.SetTopWindow(frame)
        frame.doTest()
        return True

if __name__ == "__main__":
    app = TestApp()
    app.MainLoop()
