function naive_bayes(a,b,c,d)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here
type=c;
%disp(type);
A=importdata(a);
[m,n]=size(A);
un_classes=unique(A(:,n));
[classes_number,e]=size(un_classes);
counts=zeros(classes_number,2);
%finding counts for each class
for i=1:classes_number
    count=0;
    for j=1:m
        if A(j,9)==un_classes(i,1)
            count=count+1;
        end
        counts(i,2)=un_classes(i,1);
        counts(i,1)=count;
    end
end
                                            %HISTOGRAMS
if strcmp(type,'histograms')
    rangecount=1;
    bin=str2num(d);
    %binvalues=zeros(m,n);
    binranges = zeros(classes_number*bin*(n-1),6);
    for i=1:classes_number
        z=1;
        temp_class=zeros(counts(i,1),n-1);
        for k=1:m
            if A(k,n)==un_classes(i,1)
                for y=1:n-1
                    temp_class(z,y)=A(k,y);
                end
                z=z+1;
             end
        end
        for k=1:n-1
            low=min(temp_class(:,k));
            high=max(temp_class(:,k));
            g=(high-low)/(bin-3);
            if g<0.0001
                g=0.0001;
            end
            for l=1:bin
                binranges(rangecount,1)=i;
                binranges(rangecount,2)=k;
                binranges(rangecount,3)=l;
                if l==1
                    binranges(rangecount,4)=-Inf;
                    binranges(rangecount,5)=low-(g/2);
                end
                if l==bin
                    binranges(rangecount,4)=high+(g/2);
                    binranges(rangecount,5)=Inf;
                end
                if (l~=1) && (l~=bin)
                    binranges(rangecount,4)=binranges(rangecount-1,5);
                    binranges(rangecount,5)=low+((l-2)*g)+(g/2);
                end 
                rangecount=rangecount+1;                    
            end
            
        end
    end
    for i=1:classes_number*bin*(n-1)
        class=binranges(i,1);
        dim=binranges(i,2);
        bins=binranges(i,3);
        lrange=binranges(i,4);
        hrange=binranges(i,5);
        bcount=0;
        for j=1:m
            if A(j,n)==class
                if A(j,dim)>=lrange && A(j,dim)<hrange
                    bcount=bcount+1;
                end
            end
            
        end
        for y=1:classes_number
            if counts(y,2)==class
                class_length=counts(y,1);
            end
        end
        dif=hrange-lrange;
        val=bcount/(class_length*dif);
        binranges(i,6)=val;
        fprintf('Class %d, attribute %d, bin = %d,P(bin | class) = %.2f.\n',class,dim-1,bins-1,val);       
    end
    %TESTING
    test_file=importdata(b);
    [testm,testn]=size(test_file);
    totalaccuracy=0.00;
    for i=1:testm
        
        true_class=test_file(i,testn);
        new_struct=zeros(classes_number,2);
        new_struct(:,1)=counts(:,2);
        den=0.00;
        for j=1:classes_number
            prior=counts(j,1)/m;
            for k=1:testn-1
                element=test_file(i,k);
                for l=1:classes_number*bin*(n-1)
                    if binranges(l,1)==j && binranges(l,2)==k && element>=binranges(l,4) && element<binranges(l,5)
                        prior=prior*binranges(l,6);
                    end
                end
                
            end
            den=den+prior;
            new_struct(j,2)=prior;
        end
        if den==0.00
            probable=0.00;
        else
            probable=max(new_struct(:,2));
        end
        c=0;
        for l=1:classes_number
            if new_struct(l,2)==probable
                c=c+1;
            end
        end
        
        if c>1
            competing=zeros(c,1);
        end
        d=1;
        for l=1:classes_number
            if new_struct(l,2)==probable
                competing(d,1)=new_struct(l,1);
                d=d+1;
            end
        end
        s=find(new_struct(:,2)==probable,1);
        predicted_class=new_struct(s,1);
        if c==1 && predicted_class==test_file(i,testn)
            accuracy=1.00;
        end
        if c==1 && predicted_class~=test_file(i,testn)
            accuracy=0.00;
        end
        if c>1
            for l=1:c
                if competing(l,1)==predicted_class
                    accuracy=1/c;
                else
                    accuracy=0.00;
                end
            end
        end
        totalaccuracy=totalaccuracy+accuracy;
         fprintf('ID = %5d, predicted = %3d, probability = %.4f, true = %3d,accuracy=%4.2f.\n',i-1,predicted_class,probable/den,true_class,accuracy);
         
    end  
    fin=totalaccuracy/testm;
    fprintf('classification accuracy=%6.4f\n',fin);
    %disp(fin);        
        
end
    
    
                                                    %GAUSSIANS
if strcmp(type,'gaussians')
    gaussian_values=zeros(classes_number*(n-1),4);
    g_count=1;
    for i=1:classes_number
        z=1;
        temp_class=zeros(counts(i,1),n-1);
        for k=1:m
            if A(k,n)==un_classes(i,1)
                for y=1:n-1
                    temp_class(z,y)=A(k,y);
                end
                z=z+1;
             end
         end
         for x=1:n-1
             sum=0;
             var=0;
             for y=1:counts(i,1)
                 sum=sum+temp_class(y,x);
             end
             mean=sum/counts(i,1);
             for y=1:counts(i,1)
                 var=var+((temp_class(y,x)-mean)*(temp_class(y,x)-mean));
             end
             var=var/(counts(i,1)-1);
             st=var.^(1/2);
             if st<0.01
                 st=0.01;
             end
             gaussian_values(g_count,1)=i;
             gaussian_values(g_count,2)=x;
             gaussian_values(g_count,3)=mean;
             gaussian_values(g_count,4)=st;
             
             
             fprintf('Class %d, attribute %d, mean = %.2f, std = %.2f.\n',i,x-1,mean,st); 
             %disp(g_count);
             g_count=g_count+1;
         end       
    end
    %TESTING
    gaus_test=importdata(b);
    [gm,gn]=size(gaus_test);
    totalaccuracy=0.00;
    for i=1:gm        
        true_class=gaus_test(i,gn);
        new_struct=zeros(classes_number,2);
        new_struct(:,1)=counts(:,2);
        den=0.00;
        for j=1:classes_number
            prior=counts(j,1)/m;
            for k=1:gn-1
                element=gaus_test(i,k);
                for l=1:classes_number*(gn-1)
                    if gaussian_values(l,1)==j && gaussian_values(l,2)==k
                        mn=gaussian_values(l,3);
                        std=gaussian_values(l,4);
                        pie=(2*3.1415926535897932384626).^(1/2);  
                        val=((1/(std*pie))*exp(-((element-mn)*(element-mn))/(2*std*std)));
                        prior=prior*val;
                    end
                end
                
            end
            den=den+prior;
            new_struct(j,2)=prior;
        end
        if den==0.00
            probable=0.00;
        else
            probable=max(new_struct(:,2));
        end
        c=0;
        for l=1:classes_number
            if new_struct(l,2)==probable
                c=c+1;
            end
        end
        
        if c>1
            competing=zeros(c,1);
        end
        d=1;
        for l=1:classes_number
            if new_struct(l,2)==probable
                competing(d,1)=new_struct(l,1);
                d=d+1;
            end
        end
        s=find(new_struct(:,2)==probable,1);
        predicted_class=new_struct(s,1);
        if c==1 && predicted_class==gaus_test(i,gn)
            accuracy=1.00;
        end
        if c==1 && predicted_class~=gaus_test(i,gn)
            accuracy=0.00;
        end
        if c>1
            for l=1:c
                if competing(l,1)==predicted_class
                    accuracy=1/c;
                else
                    accuracy=0.00;
                end
            end
        end
        totalaccuracy=totalaccuracy+accuracy;
         fprintf('ID = %5d, predicted = %3d, probability = %.4f, true = %3d,accuracy=%4.2f.\n',i-1,predicted_class,probable/den,true_class,accuracy);
         
    end  
    fin=totalaccuracy/gm;
    fprintf('classification accuracy=%6.4f\n',fin);
    %disp(fin);
    
end
                                                       %MIXTURES
if strcmp(type,'mixtures')
    k=str2double(d);
    
    for i=1:classes_number
        z=1;
        temp_class=zeros(counts(i,1),n-1);
        for l=1:m
            if A(l,n)==un_classes(i,1)
                for y=1:n-1
                    temp_class(z,y)=A(l,y);
                end
                z=z+1; 
             end
        end        
        for j=1:n-1
             temp_numbers=temp_class(:,j);
             mix_values=zeros(k,7);
             y=1;
             low=min(temp_class(:,j));
             greatest=max(temp_class(:,j));
             g=(greatest-low)/k;
             for l=1:k
                mix_values(y,1)=un_classes(i,1);%class
                mix_values(y,2)=j;%attribute
                mix_values(y,3)=l;%gaussian
                mix_values(y,4)=low+((l-1)*g)+(g/2);%mean
                mix_values(y,5)=1.00;%std
                mix_values(y,6)=1/k;%weight
                mix_values(y,7)=1.00;%probability
                y=y+1;                
             end
            for y=1:50
                den=0.00;
                vgaussians=zeros(counts(i,1),k+1);
                for l=1:k                  
                    for x=1:counts(i,1)
                        element=temp_numbers(x,1);
                        mn=mix_values(l,4);
                        std=mix_values(l,5);
                        pie=(2*3.1415926535897932384626).^(1/2);
                        pr=((1/(std*pie))*exp(-((element-mn)*(element-mn))/(2*std*std)));   
                        vgaussians(x,l)=((1/(std*pie))*exp(-((element-mn)*(element-mn))/(2*std*std)));
                        vgaussians(x,k+1)=vgaussians(x,k+1)+(vgaussians(x,l)*mix_values(l,6));                        
                    end
                end
                for l=1:k
                    for c=1:counts(i,1)
                        vgaussians(c,l)=vgaussians(c,l)/vgaussians(c,k+1);
                    end
                end
                wden=0.00;
                for l=1:k
                    for c=1:counts(i,1)
                        wden=wden+vgaussians(c,l);
                    end
                end
                for l=1:k
                    mean_den=0.00;
                    mean_num=0.00;                    
                    for c=1:counts(i,1)
                        mean_den=mean_den+(vgaussians(c,l));
                        mean_num=mean_num+(vgaussians(c,l)*temp_numbers(c,1));                        
                    end
                    mix_values(l,4)=mean_num/mean_den;                    
                    mix_values(l,6)=mean_den/wden;
                end  
                for l=1:k
                    std_num=0.00;
                    std_den=0.00;
                    for c=1:counts(i,1)
                        std_den=std_den+(vgaussians(c,l));
                        std_num=std_num+(vgaussians(c,l)*((temp_numbers(c,1)-mix_values(k,4))*(temp_numbers(c,1)-mix_values(k,4))));                        
                    end
                    sd=(std_num/std_den).^(1/2);
                    if sd<0.01
                        sd=0.01;
                    end
                    mix_values(l,5)=sd;
                end                
            end
            for l=1:k
                fprintf('Class %d, attribute %d, Gaussian %d, mean = %.2f, std = %.2f.\n',mix_values(l,1),mix_values(l,2)-1,mix_values(l,3)-1,mix_values(l,4),mix_values(l,5)); 
            end
        end
    end
    
    %TESTING
    mix_test=importdata(b);
    [gm,gn]=size(mix_test);
    totalaccuracy=0.00;
    for i=1:gm        
        true_class=mix_test(i,gn);
        new_struct=zeros(classes_number,2);
        new_struct(:,1)=counts(:,2);
        den=0.00;
        for j=1:classes_number
            prior=counts(j,1)/m;
            for k=1:gn-1
                element=mix_test(i,k);
                for l=1:classes_number*(gn-1)
                    if gaussian_values(l,1)==j && gaussian_values(l,2)==k
                        mn=gaussian_values(l,3);
                        std=gaussian_values(l,4);
                        pie=(2*3.1415926535897932384626).^(1/2);  
                        val=((1/(std*pie))*exp(-((element-mn)*(element-mn))/(2*std*std)));
                        prior=prior*val;
                    end
                end
                
            end
            den=den+prior;
            new_struct(j,2)=prior;
        end
        if den==0.00
            probable=0.00;
        else
            probable=max(new_struct(:,2));
        end
        c=0;
        for l=1:classes_number
            if new_struct(l,2)==probable
                c=c+1;
            end
        end
        
        if c>1
            competing=zeros(c,1);
        end
        d=1;
        for l=1:classes_number
            if new_struct(l,2)==probable
                competing(d,1)=new_struct(l,1);
                d=d+1;
            end
        end
        s=find(new_struct(:,2)==probable,1);
        predicted_class=new_struct(s,1);
        if c==1 && predicted_class==gaus_test(i,gn)
            accuracy=1.00;
        end
        if c==1 && predicted_class~=gaus_test(i,gn)
            accuracy=0.00;
        end
        if c>1
            for l=1:c
                if competing(l,1)==predicted_class
                    accuracy=1/c;
                else
                    accuracy=0.00;
                end
            end
        end
        totalaccuracy=totalaccuracy+accuracy;
         fprintf('ID = %5d, predicted = %3d, probability = %.4f, true = %3d,accuracy=%4.2f.\n',i-1,predicted_class,probable/den,true_class,accuracy);
         
    end  
    fin=totalaccuracy/gm;
    %fprintf('classification accuracy=%6.4f\n',fin);
    %disp(fin);
    
end
end

