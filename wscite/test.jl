# include("myscript/test.jl")

# Insertion Sort from "Introduction to Algorithms", pg 18
function insort!(arr)
	for j = 2:length(arr)
		key = arr[j]
		i = j - 1
		while i>0 && arr[i]>key
			arr[i+1] = arr[i]
			i = i-1
		end
		arr[i+1]=key
	end
end

# Same Insertion Sort that preserves the original array
function insort(arrorg)
	arr = copy(arrorg)
	for j = 2:length(arr)
		key = arr[j]
		i = j - 1
		while i>0 && arr[i]>key
			arr[i+1] = arr[i]
			i = i-1
		end
		arr[i+1]=key
	end
	return arr
end

# Merge Sort pg 31
function msortmerge!(A, p, q, r)
	p = Int(p)
	q = Int(q)
	r = Int(r)
	n1 = q - p + 1
	n2 = r - q
	L = zeros(n1+1)
	R = zeros(n2+1)
 
	for i=1:n1
		L[i]=A[p+i-1]
	end
	L[n1+1] = Inf
	
	for i=1:n2
		R[i]=A[q+i]
	end
	R[n2+1] = Inf
	
	j = i = 1
	for k = p:r
		if L[i]<=R[j]
			A[k] = L[i]
			i=i+1
		else
			A[k] = R[j]
			j=j+1
		end
	end
end

function msort!(A, p::Int=1, r::Int=length(A))
	p = Int(p)
	r = Int(r)
	if p < r
		q = (p+r)/2
		q = Int(floor(q))
		msort!(A, p, q)
		msort!(A, q+1, r)
		msortmerge!(A, p, q, r)
	end
end


for i=1:10
	A=rand(100_000,1)
	B=copy(A)
	C=copy(B)
	println("Run ", i)
	@time sort(C, dims = 1)
	@time msort!(A)
	@time insort!(B)
end

