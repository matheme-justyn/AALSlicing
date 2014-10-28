% Program: AALBorder.m
%          Read AAL segmentation to load border of each ROI
%
% Programmer : skylikewater - Jheng-Ting Chen, NTU GIBMS 2nd, R01454016
%              justin666666@gmail.com
%
% History:
% 141028 skylikewater - first release
%

InputPath = 'D:\Dropbox\JTWorkspace\Script\R\NTU112\AALSlicing\';
OutputPath = 'D:\Dropbox\JTWorkspace\Data\NTU112\Resource\';

AALHeader = spm_vol(fullfile(InputPath, 'aal.nii'));
AAL = spm_read_vols(AALHeader);
AALSize = size(AAL);

AALNameFile = fullfile(InputPath, 'aal.nii.txt');
AALName = importdata(AALNameFile);
AALName = AALName.textdata;

XYZ = cell(length(AALName), 6);
XStart = 0; XEnd = 0;
YStart = 0; YEnd = 0;
ZStart = 0; ZEnd = 0;
XYZHeader = {'ROIName', 'XStart', 'XEnd', 'YStart', 'YEnd', 'ZStart', 'ZEnd'};
for AALNameNum = 1:length(AALName)
  NowAALNum = str2num(AALName{AALNameNum,1});
  disp(['Now is No. ', num2str(AALNameNum), ' ROI ', AALName{AALNameNum,2}])
  % Traversal
  for XNum = 1:AALSize(1)
    if  any(any(any(AAL(XNum,:,:) == NowAALNum)))
	  XStart = XNum;
	  break
	end
  end
  for XNum = AALSize(1):-1:1
    if  any(any(any(AAL(XNum,:,:) == NowAALNum)))
	  XEnd = XNum;
	  break
	end
  end
  for YNum = 1:AALSize(2)
    if  any(any(any(AAL(:,YNum,:) == NowAALNum)))
	  YStart = YNum;
	  break
	end
  end
  for YNum = AALSize(2):-1:1
    if  any(any(any(AAL(:,YNum,:) == NowAALNum)))
	  YEnd = YNum;
	  break
	end
  end
  for ZNum = 1:AALSize(3)
    if  any(any(any(AAL(:,:,ZNum) == NowAALNum)))
	  ZStart = ZNum;
	  break
	end
  end
  for ZNum = AALSize(3):-1:1
    if  any(any(any(AAL(:,:,ZNum) == NowAALNum)))
	  ZEnd = ZNum;
	  break
	end
  end
  % input
  XYZ{AALNameNum, 1} = AALName{AALNameNum,2};
  XYZ{AALNameNum, 2} = XStart;
  XYZ{AALNameNum, 3} = XEnd;
  XYZ{AALNameNum, 4} = YStart;
  XYZ{AALNameNum, 5} = YEnd;
  XYZ{AALNameNum, 6} = ZStart;
  XYZ{AALNameNum, 7} = ZEnd;
end
XYZ = [XYZHeader; XYZ];

fid = fopen(fullfile(OutputPath, 'AALROICord.csv'),'w');
fprintf(fid, '%s,%s,%s,%s,%s,%s,%s\n', XYZ{1, :});
for AALNameNum = 2:(length(AALName)+1)
  fprintf(fid, '%s,%u,%u,%u,%u,%u,%u\n', XYZ{AALNameNum, :});
end
fclose(fid);