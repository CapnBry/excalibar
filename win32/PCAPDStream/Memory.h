class cMemory
{
public:
    cMemory();
    ~cMemory();

	HANDLE hGameProcess;
	unsigned char CryptKey[256];

	void Decrypt( unsigned char *buf, unsigned int len);
	DWORD FindGameProcess();
	unsigned long FindMemOffset(DWORD GamePID);	
	bool GetKey();
	bool CryptKeySet;
};