class cMain
{
public:
	cMain( HINSTANCE hInstance);
	bool Run();
	void StatusUpdate(const char* fmt, ...);
	bool InitMain();

private:
	HINSTANCE hInst;
	HWND hMainWnd;
	static LRESULT CALLBACK MainDlgProc( HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam );
};
