#include <iostream>
#include <fstream>
#include <string>

using namespace std;

int main() {
	ifstream paramsFile;
	paramsFile.open("params.txt");
	string temp; string params = ""; int i = 0;
	
	while (!paramsFile.eof()) {
		paramsFile >> temp;
		params += temp;
		params += "0 = -1";
		params += ',';
		i++;
		if (i > 10) {
			i = 0;
			cout << params << endl;
			params = "";
		}
	}
	cout << params << endl;
	
	/*
	while (!paramsFile.eof()) {
		paramsFile >> temp;
		cout << "if (" << temp << "0 != -1) " << "dev = filter(dev," << temp << " == "<<temp<<"0)" << endl;
	}
	*/
	

	paramsFile.close();
	return 0;
}