/****************************************************************************
** ui.h extension file, included from the uic-generated form implementation.
**
** If you wish to add, delete or rename slots use Qt Designer which will
** update this file, preserving your code. Create an init() slot in place of
** a constructor, and a destroy() slot in place of a destructor.
*****************************************************************************/

#include<qwidget.h>
#include<qtextedit.h>

void exMessagesUi::resizeEvent( QResizeEvent *e)
{
    QWidget *tab;
    int h, w;
    QTextEdit *textbox;
    int count = this->tabWidget->count();

    h = e->size().height() - 117;
    w = e->size().width() - 47;

    for( int x = 0; x < count; x++)
    {
	tab = this->tabWidget->page( x);
	textbox = (QTextEdit*)tab->childAt(10, 10);
	
	textbox->resize( w > 0 ? w : 0, h > 0 ? h : 0);
    }
}
